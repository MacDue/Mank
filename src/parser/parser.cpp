#include <mpark/patterns.hpp>

#include "ast/ast_builder.h"

#include "parser/parser.h"
#include "parser/token_helpers.h"

#include "errors/compiler_errors.h"

Ast_File Parser::parse_from_file(std::string file_path) {
  Lexer lexer;
  lexer.load_file(file_path);
  Parser parser(lexer);
  return parser.parse_file();
}

Ast_File Parser::parse_from_string(std::string source) {
  Lexer lexer;
  lexer.set_input_to_string(source);
  Parser parser(lexer);
  return parser.parse_file();
}

SourceLocation join_source_locations(SourceLocation start, SourceLocation end) {
  return SourceLocation {
    .start_line = start.start_line,
    .start_column = start.start_column,
    .end_line = end.end_line,
    .end_column = end.end_column,
    .start_char_idx = start.start_char_idx,
    .end_char_idx = end.end_char_idx,
  };
}

template<typename TAst>
SourceLocation extract_location(TAst& ast) {
  return std::visit([](auto& ast){ return ast.location; }, ast.v);
}

/* Top level constructs */

Ast_File Parser::parse_file() {
  /*
    white_space = ? any whitespace ? ;
    file = { function } ;
  */
  Ast_File parsed_file(this->lexer.input_source_name());
  this->ctx = &parsed_file.ctx;

  Token next_token;
  while (!this->peek(TokenType::LEX_EOF, next_token)) {
    if (next_token.type == TokenType::FUNCTION
      || next_token.type == TokenType::PROCEDURE
      || next_token.type == TokenType::TEST
    ) {
      parsed_file.functions.emplace_back(this->parse_function());
    } else if (next_token.type == TokenType::POD) {
      parsed_file.items.emplace_back(this->parse_pod());
    } else if (next_token.type == TokenType::ENUM) {
      parsed_file.items.emplace_back(this->parse_enum());
    } else if (next_token.type == TokenType::CONST) {
      parsed_file.global_consts.emplace_back(this->parse_const_decl());
    } else if (next_token.type == TokenType::TYPEDEF) {
      parsed_file.items.emplace_back(this->parse_type_alias());
    } else {
      throw_error_here("unexpected \"{}\", expecting a function, procedure, test, const or type");
    }
  }
  return parsed_file;
}

Item_Ptr Parser::parse_type_alias() {
  expect(TokenType::TYPEDEF);
  Ast_Type_Alias type_alias;
  auto alias_name = this->parse_identifier();
  if (!alias_name) {
    throw_error_here("expected type alias name");
  }
  type_alias.alias = *alias_name;
  expect(TokenType::ASSIGN);
  type_alias.type = this->parse_type();
  expect(TokenType::SEMICOLON);
  return ctx->new_item(type_alias);
}

Item_Ptr Parser::parse_pod() {
  /*
    pod = "pod", identifier, [braced_parameter_list] ;
  */
  Ast_Pod_Declaration parsed_pod;
  expect(TokenType::POD); // should already have been matched
  auto pod_name = this->parse_identifier();
  if (!pod_name) {
    throw_error_here("expected pod name");
  }
  parsed_pod.identifier = *pod_name;
  parsed_pod.fields = this->parse_arguments(
    TokenType::LEFT_BRACE, TokenType::RIGHT_BRACE);
  return ctx->new_item(parsed_pod);
}

Item_Ptr Parser::parse_enum() {
  /*
    (basic enum -- will extend later)
    enum = "enum", identifier, "{", [enum_members], "}" ;
    enum_members = identifier, {",", identifier} ;
  */
  Ast_Enum_Declaration parsed_enum;
  expect(TokenType::ENUM);
  auto enum_name = this->parse_identifier();
  if (!enum_name) {
    throw_error_here("expected enum name");
  }
  parsed_enum.identifier = *enum_name;

  // parsing members
  expect(TokenType::LEFT_BRACE);
  while (!peek(TokenType::RIGHT_BRACE)) {
    Ast_Enum_Declaration::Member enum_member;
    auto tag = this->parse_identifier();
    if (!tag) {
      throw_error_here("expected enum member name");
    }
    enum_member.tag = *tag;
    switch (lexer.peek_next_token().type) {
      case TokenType::LEFT_BRACE: {
        Ast_Enum_Declaration::Member::PodData pod_data;
        pod_data.fields = this->parse_arguments(
          TokenType::LEFT_BRACE, TokenType::RIGHT_BRACE);
        enum_member.data = pod_data;
        break; // TODO: tuple enum
      }
      case TokenType::LEFT_PAREN: {
        Ast_Enum_Declaration::Member::TupleData tuple_data;
        tuple_data.elements = this->parse_type_list(
          TokenType::LEFT_PAREN, TokenType::RIGHT_PAREN);
        enum_member.data = tuple_data;
        break; // TODO: pod enum
      }
      default: break; // fallthrough
    }

    parsed_enum.members.push_back(enum_member);
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(TokenType::RIGHT_BRACE);

  return ctx->new_item(parsed_enum);
}

Type_Ptr Parser::parse_function() {
  /*
    type_annotation = ":", identifier ;
    function_header = "fun", white_space, identifier, type_annotation, [parameter_list]
                    | "proc", white_space, identifier,  [parameter_list] ;
    parameter_list = "(", {identifier, type_annotation}, ")" ;
    function = function_header, block ;
  */
  auto fun_start = this->current_location();
  Ast_Function_Declaration parsed_function;
  if (consume(TokenType::FUNCTION)
    || (parsed_function.procedure = consume(TokenType::PROCEDURE))
    || (parsed_function.test = consume(TokenType::TEST))
  ) {

    auto ident = this->parse_identifier();
    if (!ident) {
      throw_error_here("\"{}\" is not a valid function name");
    }
    parsed_function.identifier = *ident;


    if (!parsed_function.test) { // tests have no params/returns
      if (!parsed_function.procedure) {
        /* return type */
        expect(TokenType::COLON);
        auto return_type = this->parse_type();
        if (!return_type) {
          throw_error_here("function return type expected");
        }
        parsed_function.return_type = return_type;
      }
      if (peek(TokenType::LEFT_PAREN)) {
        parsed_function.arguments = this->parse_arguments();
      }
    }

    if (!parsed_function.return_type) {
      parsed_function.return_type = Type::void_ty();
    }

    auto body = this->parse_block();
    if (!body) {
      throw_error_here("expected function body");
    }
    parsed_function.body = *body;
    mark_ast_location(fun_start, parsed_function);
    return ctx->new_type(parsed_function);
  } else {
    // Should be unreachable
    throw_error_here("unexpected \"{}\"m expecting function, procedure or test");
  }
}

std::vector<Ast_Argument> Parser::parse_arguments(
  TokenType left_delim, TokenType right_delim, bool insert_tvars
) {
  std::vector<Ast_Argument> arguments;
  expect(left_delim);
  while (!peek(right_delim)) {
    auto arg_name = this->parse_identifier();
    if (!arg_name) {
      throw_error_here("identifier expected");
    }
    /* arg type */
    Type_Ptr arg_type;
    if (!insert_tvars || peek(TokenType::COLON)) {
      expect(TokenType::COLON);
      arg_type = this->parse_type(insert_tvars);
      if (!arg_type) {
        throw_error_here("type name expected");
      }
    } else {
      arg_type = ctx->new_tvar();
    }

    arguments.emplace_back(Ast_Argument {
      .type = arg_type,
      .name = *arg_name
    });

    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(right_delim);
  return arguments;
}

std::optional<Ast_Block> Parser::parse_block() {
  /*
    block = "{" {statement} [expr] "}" ;
  */
  auto block_start = this->current_location();
  Ast_Block parsed_block;
  if (consume(TokenType::LEFT_BRACE)) {
    while (!peek(TokenType::RIGHT_BRACE)) {
      parsed_block.statements.emplace_back(this->parse_statement());
    }
    expect(TokenType::RIGHT_BRACE);
    if (parsed_block.statements.size() > 0) {
      if (auto final_expr = std::get_if<Ast_Expression_Statement>(
            &parsed_block.statements.back()->v)
      ) {
        parsed_block.has_final_expr = final_expr->final_expr;
      }
    }
  } else {
    return std::nullopt;
  }
  mark_ast_location(block_start, parsed_block);
  return parsed_block;
}

/* Statements */

Stmt_Ptr Parser::parse_statement() {
  /*
    statement = expr_stmt
              | return
              | for_loop
              | variable_declaration
              | assignment ;
    expr_stmt = expr, terminating_symbol ;
    return = "return", expr, ";" ;
    variable_declaration = identifier, ":", [identifier], ["=", expr], ";" ;
    assignment = identifier, "=", expr ;

    terminating_symbol = ";" | ? "}" ? (* the "}" is the previous token rather than the next *)
  */
  auto stmt_start = this->current_location();
  Stmt_Ptr stmt;
  consume(TokenType::SEMICOLON);
  if (consume(TokenType::RETURN)) {
    Ast_Return_Statement return_stmt;
    if (!peek(TokenType::SEMICOLON)) {
      auto expr = this->parse_expression();
      return_stmt.expression = expr;
    }
    expect(TokenType::SEMICOLON);
    stmt = ctx->new_stmt(return_stmt);
  } else if (peek(TokenType::FOR)) {
    stmt = this->parse_for_loop();
  } else if (peek(TokenType::BIND)) {
    stmt = this->parse_structural_binding();
  } else if (peek(TokenType::LOOP)) {
    stmt = this->parse_loop();
  } else if (peek(TokenType::WHILE)) {
    stmt = this->parse_while_loop();
  } else if (peek(TokenType::BREAK) || peek(TokenType::CONTINUE)) {
    Ast_Loop_Control loop_control;
    switch (lexer.peek_next_token().type) {
      case TokenType::BREAK:
        loop_control.type = Ast_Loop_Control::BREAK;
        break;
      case TokenType::CONTINUE:
        loop_control.type = Ast_Loop_Control::CONTINUE;
        break;
      default: assert(false); // unreachable
    }
    lexer.consume_token();
    expect(TokenType::SEMICOLON);
    stmt = ctx->new_stmt(loop_control);
  } else if (auto expr = this->parse_expression()) {
    bool simple_expression = false;
    if ((stmt = this->parse_assign(expr))) {
      // ^ stmt set there (null if not an assign)
    } else if (consume(TokenType::COLON)) {
      Ast_Variable_Declaration var_decl;
      if (auto ident = std::get_if<Ast_Identifier>(&expr->v)) {
        var_decl.variable = *ident;
      } else {
        // This is kinda a odd case... Since the code only becomes invalid
        // after we parse more context.
        throw_error_at(expr, "expression is not an identifier");
      }
      var_decl.type = this->parse_type();
      // If there's no type it should be infered
      if (consume(TokenType::ASSIGN)) {
        var_decl.initializer = this->parse_expression();
      }
      stmt = ctx->new_stmt(var_decl);
    } else {
      simple_expression = true;
      Ast_Expression_Statement expr_stmt;
      expr_stmt.expression = expr;
      stmt = ctx->new_stmt(expr_stmt);
    }
    // Hack for final expressions of blocks
    bool seen_terminator = was_previously_terminating_symbol();
    if (simple_expression && peek(TokenType::RIGHT_BRACE)) {
      std::get<Ast_Expression_Statement>(stmt->v).final_expr = true;
    } else if (!seen_terminator || peek(TokenType::SEMICOLON)) {
      expect(TokenType::SEMICOLON);
    }
  } else {
    throw_error_here("unexpected \"{}\", expecting an if, expression, or return statement");
  }
  return mark_ast_location(stmt_start, stmt);
}

static std::optional<Ast_Operator>
assign_equals_to_operator(TokenType token) {
  switch (token) {
    case TokenType::PLUS_EQUAL: return Ast_Operator::PLUS;
    case TokenType::MINUS_EQUAL: return Ast_Operator::MINUS;
    case TokenType::BITWISE_OR_EQUAL: return Ast_Operator::BITWISE_OR;
    case TokenType::BITWISE_AND_EQUAL: return Ast_Operator::BITWISE_AND;
    case TokenType::TIMES_EQUAL: return Ast_Operator::TIMES;
    case TokenType::MODULO_EQUAL: return Ast_Operator::MODULO;
    default: return std::nullopt;
  }
}

Stmt_Ptr Parser::parse_assign(Expr_Ptr lhs) {
  auto assign_start = extract_location(*lhs);
  Ast_Assign assign;
  assign.target = lhs;
  std::optional<Ast_Operator> assign_op;
  TokenType next_token = this->lexer.peek_next_token().type;
  if ((assign_op = assign_equals_to_operator(next_token))) {
    /* nothing to be done :) */
  } else if (next_token != TokenType::ASSIGN) {
    return nullptr;
  }
  this->lexer.consume_token();

  auto expression = this->parse_expression();
  if (assign_op) {
    // Desugar <op>=
    Ast_Binary_Operation assign_binop;
    assign_binop.operation = *assign_op;
    assign_binop.left = lhs;
    assign_binop.right = expression;
    expression = ctx->new_expr(assign_binop);
    expression = mark_ast_location(assign_start, expression);
  }
  assign.expression = expression;
  return ctx->new_stmt(assign);
}

#define PARSE_LOOP_BODY() ({ \
  auto body = this->parse_block();                   \
  if (!body) {                                       \
    throw_error_here("expected (braced) loop body"); \
  }                                                  \
  *body; })

Stmt_Ptr Parser::parse_for_loop() {
  /*
    for_loop = "for", white_space, identifier, [type_annotation], white_space,
               "in", expr, "..", expr, block ;
  */
  if (consume(TokenType::FOR)) {
    Ast_For_Loop for_loop;
    auto loop_variable = this->parse_identifier();
    if (!loop_variable) {
      throw_error_here("expected identifier (loop variable)");
    }
    for_loop.loop_variable = *loop_variable;
    if (consume(TokenType::COLON)) {
      for_loop.type = this->parse_type();
      if (!for_loop.type) {
        throw_error_here("expected loop variable type");
      }
    }

    expect(TokenType::IN);

    for_loop.start_range = this->parse_expression(Parser::NO_STRUCTS);
    expect(TokenType::DOUBLE_DOT);
    for_loop.end_range = this->parse_expression(Parser::NO_STRUCTS);

    for_loop.body = PARSE_LOOP_BODY();

    return ctx->new_stmt(for_loop);
  } else {
    return nullptr; // impossible
  }
}

Stmt_Ptr Parser::parse_loop() {
  expect(TokenType::LOOP);
  Ast_Loop parsed_loop;
  parsed_loop.body = PARSE_LOOP_BODY();
  return ctx->new_stmt(parsed_loop);
}

Stmt_Ptr Parser::parse_while_loop() {
  expect(TokenType::WHILE);
  Ast_While_Loop parsed_while;
  parsed_while.cond = this->parse_expression(Parser::NO_STRUCTS);
  parsed_while.body = PARSE_LOOP_BODY();
  return ctx->new_stmt(parsed_while);
}

Ast_Tuple_Binds Parser::parse_tuple_binds() {
  Ast_Tuple_Binds binding;
  auto binding_start = this->current_location();
  expect(TokenType::LEFT_PAREN);
  while (!peek(TokenType::RIGHT_PAREN)) {
    if (peek(TokenType::LEFT_PAREN)) {
      binding.binds.push_back(this->parse_tuple_binds());
    } else if (peek(TokenType::LEFT_BRACE)) {
      binding.binds.push_back(this->parse_pod_binds());
    } else {
      auto named_bind = this->parse_identifier();
      if (!named_bind) {
        throw_error_here("expected bind name");
      }
      // Require types for now
      Type_Ptr type;
      if(consume(TokenType::COLON)) {
        type = this->parse_type(true);
      } else {
        type = ctx->new_tvar();
      }
      binding.binds.push_back(Ast_Argument{
        .type = type,
        .name = *named_bind
      });
    }
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(TokenType::RIGHT_PAREN);
  mark_ast_location(binding_start, binding);
  return binding;
}

Ast_Pod_Binds Parser::parse_pod_binds() {
  Ast_Pod_Binds binding;
  auto binding_start = this->current_location();
  expect(TokenType::LEFT_BRACE);
  while (!peek(TokenType::RIGHT_BRACE)) {
    Ast_Pod_Bind bind;
    expect(TokenType::DOT);
    auto field_name = this->parse_identifier();
    if (!field_name) {
      throw_error_here("expected field name");
    }
    bind.field = *field_name;
    bool nested_bind = false;
    if (consume(TokenType::DIVIDE)) {
      auto peek_next = this->lexer.peek_next_token().type;
      switch (peek_next) {
        case TokenType::IDENT:
          std::get<Ast_Bind>(bind.replacement).name = *this->parse_identifier();
          break;
        case TokenType::LEFT_PAREN:
          bind.replacement = this->parse_tuple_binds();
          nested_bind = true;
          break;
        case TokenType::LEFT_BRACE:
          bind.replacement = this->parse_pod_binds();
          nested_bind = true;
          break;
        default:
          throw_error_here("expected replacement/bind");
      }
    }
    if (!nested_bind) {
      auto& field_bind = std::get<Ast_Bind>(bind.replacement);
      if (consume(TokenType::COLON)) {
        field_bind.type = this->parse_type(true);
      } else {
        field_bind.type = ctx->new_tvar();
      }
    }
    binding.binds.push_back(bind);
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(TokenType::RIGHT_BRACE);
  mark_ast_location(binding_start, binding);
  return binding;
}

Ast_Binding Parser::parse_binding() {
  if (peek(TokenType::LEFT_PAREN)) {
    return this->parse_tuple_binds();
  } else {
    return this->parse_pod_binds();
  }
}

Stmt_Ptr Parser::parse_structural_binding() {
  /*
    sad_binding = "bind", "(" [binding_list] ")"
    binding_list = binding, {",", binding}
    binding = (identifier, [":" type]) | ("(" binding ")")
  */
  // bind (x, y)
  // bind (x: i32, y: i32)
  // bind (x: i32, (y: i32))
  Ast_Structural_Binding parsed_sad_binding;
  expect(TokenType::BIND);
  parsed_sad_binding.bindings = this->parse_binding();
  expect(TokenType::ASSIGN);
  parsed_sad_binding.initializer = this->parse_expression();
  expect(TokenType::SEMICOLON);
  return ctx->new_stmt(parsed_sad_binding);
}

Stmt_Ptr Parser::parse_const_decl() {
  Ast_Constant_Declaration const_decl;
  expect(TokenType::CONST);
  auto ident = this->parse_identifier();
  if (!ident) {
    throw_error_here("expected constant identifier");
  }
  const_decl.constant = *ident;
  expect(TokenType::COLON);
  const_decl.type = this->parse_type(true);
  expect(TokenType::ASSIGN);
  const_decl.const_expression = this->parse_expression();
  expect(TokenType::SEMICOLON);
  return ctx->new_stmt(const_decl);
}

/* Expressions */

Expr_Ptr Parser::parse_expression(Parser::ExprFlags flags) {
  /*
    (* this ebnf is abridged to avoid describing precedence which is easier done with a table *)
    expression = literal
               | identifier
               | call
               | if
               | block
               | unary_operation
               | binary_operation
               | parenthesised_expression ;
  */
  auto expr_start = this->current_location();
  auto expr = this->parse_binary_expression(flags);
  return mark_ast_location(expr_start, expr);
}

Expr_Ptr Parser::parse_postfix_expression(Parser::ExprFlags flags) {
  auto postfix_start = this->current_location();
  auto expr = this->parse_primary_expression(flags);
  while (true) {
    mark_ast_location(postfix_start, expr);

    if (!flags.paren_delimited && peek(TokenType::LEFT_PAREN)) {
      expr = this->parse_call(std::move(expr));
    } else if (peek(TokenType::DOT)) {
      expr = this->parse_field_access(std::move(expr));
    } else if (peek(TokenType::LEFT_SQUARE_BRACKET)) {
      expr = this->parse_index_access(std::move(expr));
    } else {
      break;
    }
  }
  return mark_ast_location(postfix_start, expr);
}

std::vector<Expr_Ptr> Parser::parse_expression_list(
  TokenType left_delim, TokenType right_delim
) {
  std::vector<Expr_Ptr> expressions;
  if (left_delim != TokenType::INVALID) {
    expect(left_delim);
  }
  while (!peek(right_delim)) {
    expressions.emplace_back(this->parse_expression());
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(right_delim);
  return expressions;
}

Expr_Ptr Parser::parse_call(Expr_Ptr target) {
  /*
    call = expression, [specializations], "(", [expression_list], ")" ;
    expression_list = expression, {",", expression} ;
    specializations = "@, "(", [type_list], ")" ;
  */
  Ast_Call parsed_call;
  parsed_call.callee = std::move(target);
  parsed_call.arguments = this->parse_expression_list();
  return ctx->new_expr(parsed_call);
}

Expr_Ptr Parser::parse_field_access(Expr_Ptr object) {
  expect(TokenType::DOT);
  Ast_Field_Access parsed_access;
  parsed_access.object = std::move(object);
  auto field = this->parse_identifier();
  if (!field) {
    throw_error_here("expected field name");
  }
  parsed_access.field = *field;
  return ctx->new_expr(parsed_access);
}

Expr_Ptr Parser::parse_index_access(Expr_Ptr object) {
  expect(TokenType::LEFT_SQUARE_BRACKET);
  Ast_Index_Access parsed_index;
  parsed_index.object = std::move(object);
  parsed_index.index = this->parse_expression();
  expect(TokenType::RIGHT_SQUARE_BRACKET);
  return ctx->new_expr(parsed_index);
}

Expr_Ptr Parser::parse_primary_expression(Parser::ExprFlags flags) {
  if (peek(TokenType::LITERAL) || peek(TokenType::TRUE) || peek(TokenType::FALSE)) {
    return this->parse_literal();
  } else if (peek(TokenType::IDENT)) {
    auto ident = *this->parse_identifier();
    // There is no hope for this code.
    std::optional<Ast_Specialized_Identifier> special_ident;
    std::optional<Ast_Path> ident_path;
    // FIXME! This is just hacked in.
    // Works fine for current macro impl though.
    if (this->consume(TokenType::EXCLAMATION_MARK)) {
      Ast_Macro_Identifier macro_ident;
      *static_cast<Ast_Identifier*>(macro_ident.get_raw_self()) = ident;
      return ctx->new_expr(macro_ident);
    }
    if (this->consume(TokenType::AT)) { // @([types])
      Ast_Specialized_Identifier s_ident;
      *static_cast<Ast_Identifier*>(s_ident.get_raw_self()) = ident;
      s_ident.specializations = this->parse_type_list(
        TokenType::LEFT_PAREN, TokenType::RIGHT_PAREN);
      special_ident = s_ident;
    }

    if (this->peek(TokenType::DOUBLE_COLON)) {
      // TODO: Replace Ast_Identifiers in most places with Ast_Path
      //  -> (then maybe remove Ast_Identifier expressions)
      // FIXME: Ignore specializations for now
      // (should be okay since they'd only be needed for enum pod lits anyway)
      Ast_Path path;
      path.path.push_back(ident);
      path.location = ident.location;
      while (this->consume(TokenType::DOUBLE_COLON)) {
        auto path_section = this->parse_identifier();
        if (!path_section) { throw_error_here("expected path section"); }
        path.location = join_source_locations(path.location, path_section->location);
        path.path.push_back(*path_section);
      }
      mark_ast_location(path.path.at(0).location, path);
      ident_path = path;
    }

    if (!flags.brace_delimited && peek(TokenType::LEFT_BRACE)) {
      // Only valid in non-brace limited places (could be wrapped in parens)
      // e.g. not valid if cond or for loop ranges
      Ast_Path pod_path;
      if (ident_path) {
        pod_path = *ident_path;
      } else {
        pod_path.location = ident.location;
        pod_path.path = { ident };
      }
      return this->parse_pod_literal(pod_path,
          special_ident ? special_ident->specializations : std::vector<Type_Ptr>{});
    }

    // Insert EXPLETIVE here
    if (ident_path) {
      return ctx->new_expr(*ident_path);
    } if (special_ident) {
      return ctx->new_expr(*special_ident);
    } else {
      return ctx->new_expr(ident);
    }
  } else if (peek(TokenType::LEFT_PAREN)) {
    return this->parse_parenthesised_expression();
  } else if (peek(TokenType::IF)) {
    return this->parse_if();
  } else if (auto block = parse_block()) {
    return ctx->new_expr(*block);
  } else if (peek(TokenType::LEFT_SQUARE_BRACKET)) {
    return this->parse_array_literal();
  } else if (peek(TokenType::BACKSLASH)) {
    return this->parse_lambda();
  } else if (consume(TokenType::SPAWN)) {
    Ast_Spawn spawn;
    spawn.initializer = this->parse_expression();
    return ctx->new_expr(spawn);
  } else if (peek(TokenType::SWITCH)) {
    return this->parse_switch();
  } else {
    throw_error_here("no primary expressions start with \"{}\"");
  }
}

Expr_Ptr Parser::parse_pod_literal(
  Ast_Path pod_name, std::vector<Type_Ptr> specializations
) {
  /*
    pod_literal = identifier "{" pod_field_initializer {"," pod_field_initializer} "}" ;
    pod_field_initializer = "." identifier "=" expression  ;
  */
  Ast_Pod_Literal parsed_pod;
  parsed_pod.specializations = specializations;
  parsed_pod.pod = pod_name;
  expect(TokenType::LEFT_BRACE);
  while (!peek(TokenType::RIGHT_BRACE)) {
    expect(TokenType::DOT);
    auto field_name = this->parse_identifier();
    if (!field_name) {
      throw_error_here("expected field name");
    }
    expect(TokenType::ASSIGN);
    parsed_pod.fields.push_back(PodFieldInitializer{
      .field = *field_name,
      .initializer = this->parse_expression()
    });
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(TokenType::RIGHT_BRACE);
  return ctx->new_expr(parsed_pod);
}

Expr_Ptr Parser::parse_tuple_literal(Expr_Ptr first_element) {
  /*
    tuple_literal = "(" expression "," [expression_list] ")"
                  | "(" ")" ;
  */
  Ast_Tuple_Literal parsed_tuple;
  parsed_tuple.elements.push_back(first_element);
  while (!peek(TokenType::RIGHT_PAREN)) {
    expect(TokenType::COMMA);
    if (peek(TokenType::RIGHT_PAREN)) break; // Allow optional trailing comma
    parsed_tuple.elements.push_back(this->parse_expression());
  }
  expect(TokenType::RIGHT_PAREN);
  return ctx->new_expr(parsed_tuple);
}

Expr_Ptr Parser::parse_parenthesised_expression() {
  /*
    parenthesised_expression = "(" expression ")" ;
  */
  using namespace mpark::patterns;
  expect(TokenType::LEFT_PAREN);
  if (consume(TokenType::RIGHT_PAREN)) {
    return ctx->new_expr(Ast_Tuple_Literal{}); // empty tuple
  }
  auto expr = this->parse_expression();
  if (peek(TokenType::COMMA)) { // >= 1 element tuple
    return this->parse_tuple_literal(expr);
  }
  expect(TokenType::RIGHT_PAREN);
  match(expr->v)(
    pattern(as<Ast_Binary_Operation>(arg)) = [](auto & binary_op) {
      binary_op.parenthesised = true;
    },
    pattern(_) = [] {});
  return expr;
}

Expr_Ptr Parser::parse_if() {
  /*
    if = "if", expression, block, ("else", block) | ("else", white_space, if) ;
  */
  Ast_If_Expr parsed_if;
  if (consume(TokenType::IF)) {
    auto condition = this->parse_expression(Parser::NO_STRUCTS);
    if (!condition) {
      throw_error_here("unexpected \"{}\", expecting a condition expression");
    }
    parsed_if.cond = condition;
    auto then_block = this->parse_block();
    if (!then_block) {
      throw_error_here("expected (braced) then block");
    }
    parsed_if.then_block = ctx->new_expr(*then_block);
    if (consume(TokenType::ELSE)) {
      parsed_if.has_else = true;

      if (peek(TokenType::IF)) {
        /*
        support for:
        if () {
          ...
        } else if {
          ...
        }

        rather than:
        if () {

        } else {
          if () {
            ...
          }
        }
        */
        parsed_if.else_block = this->parse_expression();
      } else {
        auto else_block = this->parse_block();
        if (!else_block) {
          throw_error_here("expected (braced) else block");
        }
        parsed_if.else_block = ctx->new_expr(*else_block);
      }
    }
    return ctx->new_expr(parsed_if);
  } else {
    return nullptr;
  }
}

/* TODO: Consider replacing with pratt parser */

static int get_binary_precdence(Ast_Operator op) {
  // Based off https://en.cppreference.com/w/c/language/operator_precedence
  switch (op)
  {
    case Ast_Operator::TIMES:
    case Ast_Operator::DIVIDE:
    case Ast_Operator::MODULO:
      return 100;
    case Ast_Operator::PLUS:
    case Ast_Operator::MINUS:
      return 90;
    case Ast_Operator::LEFT_SHIFT:
    case Ast_Operator::RIGHT_SHIFT:
      return 80;
    case Ast_Operator::LESS_THAN:
    case Ast_Operator::LESS_EQUAL:
    case Ast_Operator::GREATER_THAN:
    case Ast_Operator::GREATER_EQUAL:
      return 70;
    case Ast_Operator::EQUAL_TO:
    case Ast_Operator::NOT_EQUAL_TO:
      return 60;
    case Ast_Operator::BITWISE_AND:
      return 50;
    case Ast_Operator::BITWISE_XOR:
      return 40;
    case Ast_Operator::BITWISE_OR:
      return 30;
    case Ast_Operator::LOGICAL_AND:
      return 20;
    case Ast_Operator::LOGICAL_OR:
      return 10;
    default: return -1;
  }
}

static bool is_binary_op(TokenType token_type) {
  return get_binary_precdence(static_cast<Ast_Operator>(token_type)) > 0;
}

static bool is_unary_op(TokenType token) {
  switch (token) {
    case TokenType::PLUS:
    case TokenType::MINUS:
    case TokenType::BITWISE_NOT:
    case TokenType::LOGICAL_NOT:
    case TokenType::REF:
      return true;
    default: return false;
  }
}

// Very simple precedence/association as discussed by Jonathan Blow in a stream about Jai
static Expr_Ptr fix_precedence_and_association(
  AstContext& ctx,
  Expr_Ptr lhs,
  Expr_Ptr rhs,
  Ast_Operator op
) {
  using namespace mpark::patterns;
  return match(rhs->v)(
    pattern(as<Ast_Binary_Operation>(arg)) = [&](auto & rhs_binop) {
      WHEN(!rhs_binop.parenthesised
        && get_binary_precdence(rhs_binop.operation) <= get_binary_precdence(op)
      ) {
        /*
          Simple example:
          lhs: 3 *
          rhs: 1 + 2
          * > +
          rhs.left = fix(lhs, rhs.left) + 2
                  -> (3 * 1) + 2
        */
        rhs_binop.left = fix_precedence_and_association(ctx, lhs, rhs_binop.left, op);
        return rhs;
      };
    },
    pattern(_) = [&] {
      Ast_Binary_Operation new_binop;
      std::visit(
        [&](auto& lhs, auto& rhs) {
          new_binop.location = join_source_locations(lhs.location, rhs.location);
        }, lhs->v, rhs->v);
      new_binop.left = lhs;
      new_binop.right = rhs;
      new_binop.operation = op;
      return ctx.new_expr(new_binop);
    });
}

Expr_Ptr Parser::parse_binary_expression(Parser::ExprFlags flags) {
  /*
    binary_operation = expression, operation, expression ;
    operation = (* use your imagination *) ;
  */
  auto lhs = this->parse_unary(flags);
  if (consume(TokenType::AS)) {
    // FIXME: Hack special case: "as" cast
    Ast_As_Cast parsed_as_cast;
    parsed_as_cast.object = lhs;
    parsed_as_cast.type = this->parse_type();
    if (!parsed_as_cast.type) {
      throw_error_here("expected target type");
    }
    lhs = ctx->new_expr(parsed_as_cast);
  }
  auto bin_op = this->lexer.peek_next_token().type;
  if (is_binary_op(bin_op)) {
    lexer.consume_token();
    auto rhs = this->parse_expression(flags);
    lhs = fix_precedence_and_association(*ctx, lhs, rhs, static_cast<Ast_Operator>(bin_op));
  }
  return lhs;
}

Expr_Ptr Parser::parse_unary(Parser::ExprFlags flags) {
  /*
    unary_operation = operation, expression ;
  */
  auto unary_start = this->current_location();
  auto unary_op = this->lexer.peek_next_token().type;
  // FIXME: Quick hack for ¬ and ! nots
  if (unary_op == TokenType::EXCLAMATION_MARK) {
    unary_op = TokenType::LOGICAL_NOT;
  }
  if (!is_unary_op(unary_op)) {
    return this->parse_postfix_expression(flags);
  }
  Ast_Unary_Operation parsed_unary;
  parsed_unary.operation = static_cast<Ast_Operator>(unary_op);
  this->lexer.consume_token();
  // For nested unary expressions e.g. -------------10 (if you want that?)
  parsed_unary.operand = this->parse_unary(flags);
  auto unary = ctx->new_expr(parsed_unary);
  return mark_ast_location(unary_start, unary);
}

Expr_Ptr Parser::parse_literal() {
  /*
    literal = string_literal
            = integer
            = floating_point ;
  */
  auto& token = this->lexer.peek_next_token();

  Ast_Literal parsed_literal;
  parsed_literal.location = token.location;
  parsed_literal.value = std::string(token.raw_token);

  if (token.type != TokenType::TRUE && token.type != TokenType::FALSE) {
    parsed_literal.literal_type = token.literal_type;
    // if (token.literal_type == PrimativeType::STRING) {
    //   // Remove ""s
    //   parsed_literal.value
    //     = parsed_literal.value.substr(1, parsed_literal.value.length() - 2);
    // }
  } else {
    parsed_literal.literal_type = PrimativeType::BOOL;
  }

  this->lexer.consume_token();
  return ctx->new_expr(parsed_literal);
}

std::optional<Ast_Identifier> Parser::parse_identifier() {
  Token token;
  if (this->peek(TokenType::IDENT, token)) {
    this->lexer.consume_token();
    return Ast_Identifier(token.location, std::string(token.raw_token));
  }
  return std::nullopt;
}

Expr_Ptr Parser::parse_array_literal() {
  /*
    array_literal = "[" [expression_list] "]" ;
    repeat_array_literal "[", "=", expression, ";", expression "]" ;
  */
  expect(TokenType::LEFT_SQUARE_BRACKET);
  if (consume(TokenType::ASSIGN)) {
    Ast_Array_Repeat parsed_repeat;
    parsed_repeat.initializer = this->parse_expression();
    expect(TokenType::SEMICOLON);
    parsed_repeat.repetitions = this->parse_expression();
    expect(TokenType::RIGHT_SQUARE_BRACKET);
    return ctx->new_expr(parsed_repeat);
  } else {
    Ast_Array_Literal parsed_array;
    parsed_array.elements = this->parse_expression_list(
      TokenType::INVALID,
      TokenType::RIGHT_SQUARE_BRACKET);
    return ctx->new_expr(parsed_array);
  }
}

Expr_Ptr Parser::parse_lambda() {
  Ast_Lambda parsed_lambda;
  parsed_lambda.arguments = this->parse_arguments(
    TokenType::BACKSLASH, TokenType::ARROW, true);
  parsed_lambda.return_type = this->parse_type(true);
  auto body = this->parse_block();
  if (!body) {
    throw_error_here("expected lambda body");
  }
  parsed_lambda.body = *body;
  return ctx->new_expr(parsed_lambda);
}

Expr_Ptr Parser::parse_switch() {
  Ast_Switch_Expr parsed_switch;
  expect(TokenType::SWITCH);
  parsed_switch.switched = this->parse_expression(Parser::NO_STRUCTS);
  expect(TokenType::LEFT_BRACE);
  while (!peek(TokenType::RIGHT_BRACE)) {
    SwitchCase switch_case;
    if (!(switch_case.is_default_case = consume(TokenType::ELSE))) {
      switch_case.match = this->parse_expression(Parser::NO_CALLS_OR_STRUCTS);
      if (!peek(TokenType::FAT_ARROW)) {
        switch_case.bindings = this->parse_binding();
      }
    }
    expect(TokenType::FAT_ARROW);
    auto body = this->parse_block();
    if (!body) {
      throw_error_here("expected switch body");
    }
    switch_case.body = *body;
    parsed_switch.cases.push_back(switch_case);
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(TokenType::RIGHT_BRACE);
  return ctx->new_expr(parsed_switch);
}

/* Types */

Type_Ptr Parser::parse_type(bool default_tvar) {
  // Type modifiers
  if (consume(TokenType::REF)) {
    // TODO: ref infer
    ReferenceType ref_type;
    ref_type.references = this->parse_base_type(default_tvar);
    if (!ref_type.references) {
      throw_error_here("expected referenced type");
    }
    return ctx->new_type(ref_type);
  }
  return this->parse_base_type(default_tvar);
}

Type_Ptr Parser::parse_base_type(bool default_tvar) {
  if (peek(TokenType::BACKSLASH)) {
    return this->parse_lambda_type();
  } else if (peek(TokenType::LEFT_PAREN)) {
    return this->parse_tuple_type();
  }

  Type_Ptr type;
  if (consume(TokenType::BITWISE_OR)) {
    // Allows typing |\i32->i32|[] for a vector of lambdas and stuff
    // TODO: Change to parse_type()? Would allow for more odd refs...
    type = this->parse_base_type();
    expect(TokenType::BITWISE_OR);
  } else {
    // Array/simple types
    auto type_name = this->parse_identifier();
    if (!type_name) {
      if (default_tvar) {
        type = ctx->new_tvar();
      } else {
        return nullptr;
      }
    } else {
      type = ctx->new_type(UncheckedType(*type_name));
    }
  }

  if (peek(TokenType::LEFT_SQUARE_BRACKET)) {
    return this->parse_array_type(type);
  } else {
    return type;
  }
}

Type_Ptr Parser::parse_list_type(Type_Ptr base_type) {
  // i32[][]
  expect(TokenType::RIGHT_SQUARE_BRACKET);
  ListType list_type;
  if (peek(TokenType::LEFT_SQUARE_BRACKET)) {
    list_type.element_type = parse_array_type(base_type);
  } else {
    list_type.element_type = base_type;
  }
  return ctx->new_type(list_type);
}


Type_Ptr Parser::parse_array_type(Type_Ptr base_type) {
  // TODO: Array sizes should be 64bit
  // Maybe this should not parse to a fixed array type but some more abstract type
  // that can be checked later?
  static auto token_to_uint = [this](Token& token) {
    try {
      return static_cast<uint32_t>(std::stoul(std::string(token.raw_token)));
    } catch (...) {
      throw_error_here("not an u32 literal");
    }
  };
  FixedSizeArrayType top_array_type;
  auto current_array = top_array_type.get_raw_self();
  expect(TokenType::LEFT_SQUARE_BRACKET);
  while (true) {
    auto& token = lexer.peek_next_token();
    if (token.type != TokenType::LITERAL) {
      // i32[]
      //     ^-- You are here (if this is a valid parse)
      return parse_list_type(base_type);
    }
    if (token.literal_type != PrimativeType::INTEGER) {
      throw_error_here("expected array size");
    }
    current_array->size = token_to_uint(token);
    this->lexer.consume_token();
    if (!consume(TokenType::COMMA)) {
      break;
    }
    current_array->element_type = ctx->new_type(FixedSizeArrayType());
    current_array = std::get<FixedSizeArrayType>(
      current_array->element_type->v).get_raw_self();
  }
  current_array->element_type = base_type;
  expect(TokenType::RIGHT_SQUARE_BRACKET);
  return ctx->new_type(top_array_type);
}

std::vector<Type_Ptr> Parser::parse_type_list(TokenType left_delim, TokenType right_delim) {
  std::vector<Type_Ptr> type_list;
  expect(left_delim);
  while (!peek(right_delim)) {
    type_list.emplace_back(
      this->parse_type()); // FIXME: Null here?
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(right_delim);
  return type_list;
}

Type_Ptr Parser::parse_lambda_type() {
  LambdaType lambda_type;
  lambda_type.argument_types = parse_type_list(TokenType::BACKSLASH, TokenType::ARROW);
  lambda_type.return_type = this->parse_type();
  return ctx->new_type(lambda_type);
}

Type_Ptr Parser::parse_tuple_type() {
  TupleType tuple_type;
  tuple_type.element_types = parse_type_list(TokenType::LEFT_PAREN, TokenType::RIGHT_PAREN);
  return ctx->new_type(tuple_type);
}

/* Helpers */

bool Parser::consume(TokenType token_type) {
  if (this->peek(token_type)) {
    this->lexer.consume_token();
    return true;
  }
  return false;
}

void Parser::expect(TokenType token_type) {
  if (!this->consume(token_type)) {
    throw_error_here("unexpected \"{}\", was expected a {}",
      token_type_to_string(token_type));
  }
}

bool Parser::peek(TokenType token_type) {
  return this->lexer.peek_next_token().type == token_type;
}

bool Parser::peek(TokenType token_type, Token& token) {
  bool ret = this->peek(token_type);
  token = this->lexer.peek_next_token();
  return ret;
}
