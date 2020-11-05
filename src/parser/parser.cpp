#include <mpark/patterns.hpp>

#include "ast/ast_builder.h"

#include "parser/parser.h"
#include "parser/token_helpers.h"

#include "sema/sema_errors.h"

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
  Ast_File parsed_file;
  parsed_file.filename = this->lexer.input_source_name();
  Token next_token;
  while (!this->peek(TokenType::LEX_EOF, next_token)) {
    if (next_token.type == TokenType::FUNCTION
      || next_token.type == TokenType::PROCEDURE
    ) {
      parsed_file.functions.emplace_back(this->parse_function());
    } else if (next_token.type == TokenType::POD) {
      parsed_file.pods.emplace_back(this->parse_pod());
    } else {
      throw_error_here("unexpected \"{}\", expecting a function, procedure or pod type");
    }
  }
  return parsed_file;
}

Type_Ptr Parser::parse_pod() {
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
  return std::make_shared<Type>(parsed_pod);
}

Function_Ptr Parser::parse_function() {
  /*
    type_annotation = ":", identifier ;
    function_header = "fun", white_space, identifier, type_annotation, [parameter_list]
                    | "proc", white_space, identifier,  [parameter_list] ;
    parameter_list = "(", {identifier, type_annotation}, ")" ;
    function = function_header, block ;
  */
  Ast_Function_Declaration parsed_function;
  if (consume(TokenType::FUNCTION) || (parsed_function.procedure = consume(TokenType::PROCEDURE))) {

    auto ident = this->parse_identifier();
    if (!ident) {
      throw_error_here("\"{}\" is not a valid function name");
    }
    parsed_function.identifier = *ident;
    if (!parsed_function.procedure) {
      /* return type */
      expect(TokenType::COLON);
      auto return_type = this->parse_type();
      if (!return_type) {
        throw_error_here("function return type expected");
      } else {
        parsed_function.return_type = return_type;
      }
    }

    if (peek(TokenType::LEFT_PAREN)) {
      parsed_function.arguments = this->parse_arguments();
    }

    auto body = this->parse_block();
    if (!body) {
      throw_error_here("expected function body");
    }
    parsed_function.body = *body;
    return std::make_shared<Type>(parsed_function);
  } else {
    // Should be unreachable
    throw_error_here("unexpected \"{}\"m expecting function or procedure");
  }
}

std::vector<Ast_Argument> Parser::parse_arguments(
  TokenType left_delim, TokenType right_delim
) {
  std::vector<Ast_Argument> arguments;
  expect(left_delim);
  while (!peek(right_delim)) {
    auto arg_name = this->parse_identifier();
    if (!arg_name) {
      throw_error_here("identifier expected");
    }
    /* arg type */
    expect(TokenType::COLON);
    auto arg_type = this->parse_type();
    if (!arg_type) {
      throw_error_here("type name expected");
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

Statement_Ptr Parser::parse_statement() {
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
  Statement_Ptr stmt;
  consume(TokenType::SEMICOLON);
  if (consume(TokenType::RETURN)) {
    Ast_Return_Statement return_stmt;
    if (!peek(TokenType::SEMICOLON)) {
      auto expr = this->parse_expression();
      return_stmt.expression = expr;
    }
    expect(TokenType::SEMICOLON);
    stmt = to_stmt_ptr(return_stmt);
  } else if (peek(TokenType::FOR)) {
    stmt = this->parse_for_loop();
  } else if (auto expr = this->parse_expression()) {
    bool simple_expression = false;
    if (stmt = this->parse_assign(expr)) {
      // ^ stmt set there (null if not an assign)
    } else if (consume(TokenType::COLON)) {
      Ast_Variable_Declaration var_decl;
      if (auto ident = std::get_if<Ast_Identifier>(&expr->v)) {
        var_decl.variable = *ident;
      } else {
        // This is kinda a odd case... Since the code only becomes invalid
        // after we parse more context.
        throw_sema_error_at(expr, "expression is not an identifier");
      }
      var_decl.type = this->parse_type();
      // If there's no type it should be infered
      if (consume(TokenType::ASSIGN)) {
        var_decl.initializer = this->parse_expression();
      }
      stmt = to_stmt_ptr(var_decl);
    } else {
      simple_expression = true;
      Ast_Expression_Statement expr_stmt;
      expr_stmt.expression = expr;
      stmt = to_stmt_ptr(expr_stmt);
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

Statement_Ptr Parser::parse_assign(Expression_Ptr lhs) {
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
    expression = to_expr_ptr(assign_binop);
    expression = mark_ast_location(assign_start, expression);
  }
  assign.expression = expression;
  return to_stmt_ptr(assign);
}

Statement_Ptr Parser::parse_for_loop() {
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

    for_loop.start_range = this->parse_expression();
    expect(TokenType::DOUBLE_DOT);
    for_loop.end_range = this->parse_expression();

    auto body = this->parse_block();

    if (!body) {
      throw_error_here("expected (braced) loop body");
    }
    for_loop.body = *body;

    return to_stmt_ptr(for_loop);
  } else {
    return nullptr; // impossible
  }
}

/* Expressions */

Expression_Ptr Parser::parse_expression() {
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
  auto expr = this->parse_binary_expression();
  return mark_ast_location(expr_start, expr);
}

Expression_Ptr Parser::parse_postfix_expression() {
  auto postfix_start = this->current_location();
  auto expr = this->parse_primary_expression();
  while (true) {
    mark_ast_location(postfix_start, expr);

    if (peek(TokenType::LEFT_PAREN)) {
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

std::vector<Expression_Ptr> Parser::parse_expression_list(
  TokenType left_delim, TokenType right_delim
) {
  std::vector<Expression_Ptr> expressions;
  expect(left_delim);
  while (!peek(right_delim)) {
    expressions.emplace_back(this->parse_expression());
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(right_delim);
  return expressions;
}

Expression_Ptr Parser::parse_call(Expression_Ptr target) {
  /*
    call = expression, "(", [expression_list], ")" ;
    expression_list = expression, {",", expression} ;
  */
  Ast_Call parsed_call;
  parsed_call.callee = std::move(target);
  parsed_call.arguments = this->parse_expression_list();
  return to_expr_ptr(parsed_call);
}

Expression_Ptr Parser::parse_field_access(Expression_Ptr object) {
  expect(TokenType::DOT);
  Ast_Field_Access parsed_access;
  parsed_access.object = std::move(object);
  auto field = this->parse_identifier();
  if (!field) {
    throw_error_here("expected field name");
  }
  parsed_access.field = *field;
  return to_expr_ptr(parsed_access);
}

Expression_Ptr Parser::parse_index_access(Expression_Ptr object) {
  expect(TokenType::LEFT_SQUARE_BRACKET);
  Ast_Index_Access parsed_index;
  parsed_index.object = std::move(object);
  parsed_index.index = this->parse_expression();
  expect(TokenType::RIGHT_SQUARE_BRACKET);
  return to_expr_ptr(parsed_index);
}

Expression_Ptr Parser::parse_primary_expression() {
  if (peek(TokenType::LITERAL) || peek(TokenType::TRUE) || peek(TokenType::FALSE)) {
    return this->parse_literal();
  } else if (peek(TokenType::IDENT)) {
    return to_expr_ptr(*this->parse_identifier());
  } else if (peek(TokenType::LEFT_PAREN)) {
    return this->parse_parenthesised_expression();
  } else if (peek(TokenType::IF)) {
    return this->parse_if();
  } else if (auto block = parse_block()) {
    return to_expr_ptr(*block);
  } else if (peek(TokenType::LEFT_SQUARE_BRACKET)) {
    return this->parse_array_literal();
  } else {
    throw_error_here("no primary expressions start with \"{}\"");
  }
}

Expression_Ptr Parser::parse_parenthesised_expression() {
  /*
    parenthesised_expression = "(" expression ")" ;
  */
  using namespace mpark::patterns;
  expect(TokenType::LEFT_PAREN);
  auto expr = this->parse_expression();
  expect(TokenType::RIGHT_PAREN);
  match(expr->v)(
    pattern(as<Ast_Binary_Operation>(arg)) = [](auto & binary_op) {
      binary_op.parenthesised = true;
    },
    pattern(_) = [] {});
  return expr;
}

Expression_Ptr Parser::parse_if() {
  /*
    if = "if", expression, block, ("else", block) | ("else", white_space, if) ;
  */
  Ast_If_Expr parsed_if;
  if (consume(TokenType::IF)) {
    auto condition = this->parse_expression();
    if (!condition) {
      throw_error_here("unexpected \"{}\", expecting a condition expression");
    }
    parsed_if.cond = condition;
    auto then_block = this->parse_block();
    if (!then_block) {
      throw_error_here("expected (braced) then block");
    }
    parsed_if.then_block = to_expr_ptr(*then_block);
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
        parsed_if.else_block = to_expr_ptr(*else_block);
      }
    }
    return to_expr_ptr(parsed_if);
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
      return true;
    default: return false;
  }
}

// Very simple precedence/association as discussed by Jonathan Blow in a stream about Jai
static Expression_Ptr fix_precedence_and_association(
  Expression_Ptr lhs,
  Expression_Ptr rhs,
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
        rhs_binop.left = fix_precedence_and_association(
          std::move(lhs), std::move(rhs_binop.left), op);
        return rhs;
      };
    },
    pattern(_) = [&] {
      Ast_Binary_Operation new_binop;
      std::visit(
        [&](auto& lhs, auto& rhs) {
          new_binop.location = join_source_locations(lhs.location, rhs.location);
        }, lhs->v, rhs->v);
      new_binop.left = std::move(lhs);
      new_binop.right = std::move(rhs);
      new_binop.operation = op;
      return to_expr_ptr(new_binop);
    });
}

Expression_Ptr Parser::parse_binary_expression() {
  /*
    binary_operation = expression, operation, expression ;
    operation = (* use your imagination *) ;
  */
  auto lhs = this->parse_unary();
  auto bin_op = this->lexer.peek_next_token().type;
  if (is_binary_op(bin_op)) {
    lexer.consume_token();
    auto rhs = this->parse_expression();
    lhs = fix_precedence_and_association(
      std::move(lhs), std::move(rhs), static_cast<Ast_Operator>(bin_op));
  }
  return lhs;
}

Expression_Ptr Parser::parse_unary() {
  /*
    unary_operation = operation, expression ;
  */
  auto unary_start = this->current_location();
  auto unary_op = this->lexer.peek_next_token().type;
  if (!is_unary_op(unary_op)) {
    return this->parse_postfix_expression();
  }
  Ast_Unary_Operation parsed_unary;
  parsed_unary.operation = static_cast<Ast_Operator>(unary_op);
  this->lexer.consume_token();
  // For nested unary expressions e.g. -------------10 (if you want that?)
  parsed_unary.operand = this->parse_unary();
  auto unary = to_expr_ptr(parsed_unary);
  return mark_ast_location(unary_start, unary);
}

Expression_Ptr Parser::parse_literal() {
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
    if (token.literal_type == PrimativeType::STRING) {
      // Remove ""s
      parsed_literal.value
        = parsed_literal.value.substr(1, parsed_literal.value.length() - 2);
    }
  } else {
    parsed_literal.literal_type = PrimativeType::BOOL;
  }

  this->lexer.consume_token();
  return to_expr_ptr(parsed_literal);
}

std::optional<Ast_Identifier> Parser::parse_identifier() {
  Token token;
  if (this->peek(TokenType::IDENT, token)) {
    this->lexer.consume_token();
    return Ast_Identifier(token.location, std::string(token.raw_token));
  }
  return std::nullopt;
}

Expression_Ptr Parser::parse_array_literal() {
  /*
    array_literal = "[" [expression_list] "]" ;
  */
  Ast_Array_Literal parsed_array;
  parsed_array.elements = this->parse_expression_list(
    TokenType::LEFT_SQUARE_BRACKET,
    TokenType::RIGHT_SQUARE_BRACKET);
  return to_expr_ptr(parsed_array);
}

/* Types */

Type_Ptr Parser::parse_type() {
  // Type modifiers
  if (consume(TokenType::REF)) {
    ReferenceType ref_type;
    ref_type.references = this->parse_base_type();
    return std::make_shared<Type>(ref_type);
  }
  return this->parse_base_type();
}

Type_Ptr Parser::parse_base_type() {
  auto type_name = this->parse_identifier();
  if (type_name) {
    auto type = std::make_shared<Type>(UncheckedType{*type_name});
    if (peek(TokenType::LEFT_SQUARE_BRACKET)) {
      return this->parse_array_type(type);
    } else {
      return type;
    }
  }
  return nullptr;
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
  FixedSizeArrayType* current_array = &top_array_type;
  expect(TokenType::LEFT_SQUARE_BRACKET);
  while (true) {
    auto& token = lexer.peek_next_token();
    if (token.literal_type != PrimativeType::INTEGER) {
      throw_error_here("expected array size");
    }
    current_array->size = token_to_uint(token);
    this->lexer.consume_token();
    if (!consume(TokenType::COMMA)) {
      break;
    }
    current_array->element_type = std::make_shared<Type>(FixedSizeArrayType());
    current_array = &std::get<FixedSizeArrayType>(current_array->element_type->v);
  }
  current_array->element_type = base_type;
  expect(TokenType::RIGHT_SQUARE_BRACKET);
  return std::make_shared<Type>(top_array_type);
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
