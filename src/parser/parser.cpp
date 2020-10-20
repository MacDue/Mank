#include <mpark/patterns.hpp>

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
    } else {
      throw_error_here("unexpected \"{}\", expecting a function or procedure");
    }
  }
  return parsed_file;
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
    parsed_function.identifer = *ident;
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

    if (consume(TokenType::LEFT_PAREN)) {
      /* Parameters */
      while (!peek(TokenType::RIGHT_PAREN)) {
        auto arg_name = this->parse_identifier();
        if (!arg_name) {
          throw_error_here("{} argument expected",
            parsed_function.procedure ? "procedure" : "function");
        }
        /* arg type */
        expect(TokenType::COLON);
        auto arg_type = this->parse_type();
        if (!arg_type) {
          throw_error_here("type name expected");
        }

        parsed_function.arguments.emplace_back(Ast_Argument {
          .type = arg_type,
          .name = *arg_name
        });

        if (!consume(TokenType::COMMA)) {
          break;
        }
      }
      expect(TokenType::RIGHT_PAREN);
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
    stmt = std::make_shared<Ast_Statement>(return_stmt);
  } else if (peek(TokenType::FOR)) {
    stmt = this->parse_for_loop();
  } else if (auto expr = this->parse_expression()) {
    bool simple_expression = false;
    if (consume(TokenType::ASSIGN)) {
      Ast_Assign assign;
      assign.target = expr;
      assign.expression = this->parse_expression();
      stmt = std::make_shared<Ast_Statement>(assign);
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
      stmt = std::make_shared<Ast_Statement>(var_decl);
    } else {
      simple_expression = true;
      Ast_Expression_Statement expr_stmt;
      expr_stmt.expression = expr;
      stmt = std::make_shared<Ast_Statement>(expr_stmt);
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

Statement_Ptr Parser::parse_for_loop() {
  /*
    for_loop = "for", white_space, identifier, [type_annotation], white_space,
               "in", expr, "..", expr, block ;
  */
  if (consume(TokenType::FOR)) {
    Ast_For_Loop for_loop;
    auto loop_value = this->parse_identifier();
    if (!loop_value) {
      throw_error_here("expected identifier (loop variable)");
    }
    for_loop.loop_value = *loop_value;
    if (consume(TokenType::COLON)) {
      for_loop.value_type = this->parse_type();
      if (!for_loop.value_type) {
        throw_error_here("expected loop value type");
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

    return std::make_shared<Ast_Statement>(for_loop);
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
    } else {
      break;
    }
  }
  return mark_ast_location(postfix_start, expr);
}

Expression_Ptr Parser::parse_call(Expression_Ptr target) {
  /*
    call = expression, "(", [expression_list], ")" ;
    expression_list = expression, {",", expression} ;
  */
  expect(TokenType::LEFT_PAREN);
  Ast_Call parsed_call;
  parsed_call.callee = std::move(target);
  while (!peek(TokenType::RIGHT_PAREN)) {
    parsed_call.arguments.emplace_back(this->parse_expression());
    if (!consume(TokenType::COMMA)) {
      break;
    }
  }
  expect(TokenType::RIGHT_PAREN);
  return std::make_shared<Ast_Expression>(parsed_call);
}

Expression_Ptr Parser::parse_primary_expression() {
  if (peek(TokenType::LITERAL) || peek(TokenType::TRUE) || peek(TokenType::FALSE)) {
    return this->parse_literal();
  } else if (peek(TokenType::IDENT)) {
    return std::make_shared<Ast_Expression>(*this->parse_identifier());
  } else if (peek(TokenType::LEFT_PAREN)) {
    return this->parse_parenthesised_expression();
  } else if (peek(TokenType::IF)) {
    return this->parse_if();
  } else if (auto block = parse_block()) {
    return std::make_shared<Ast_Expression>(*block);
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
    parsed_if.then_block = std::make_shared<Ast_Expression>(*then_block);
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
        parsed_if.else_block = std::make_shared<Ast_Expression>(*else_block);
      }
    }
    return std::make_shared<Ast_Expression>(parsed_if);
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
      return std::make_shared<Ast_Expression>(new_binop);
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
  auto unary = std::make_shared<Ast_Expression>(parsed_unary);
  return mark_ast_location(unary_start, unary);
}

Expression_Ptr Parser::parse_literal() {
  /*
    literal = string_literal
            = integer
            = floating_point ;
  */
  auto& token = this->lexer.peek_next_token();

  std::string literal_value(token.raw_token);
  PrimativeType::Tag literal_type;
  if (token.type != TokenType::TRUE && token.type != TokenType::FALSE) {
    literal_type = token.literal_type;
    if (token.literal_type == PrimativeType::STRING) {
      // Remove ""s
      literal_value = literal_value.substr(1, literal_value.length() - 2);
    }
  } else {
    literal_type = PrimativeType::BOOL;
  }
  Ast_Literal parsed_literal(token.location, literal_value, literal_type);

  this->lexer.consume_token();
  return std::make_shared<Ast_Expression>(parsed_literal);
}

std::optional<Ast_Identifier> Parser::parse_identifier() {
  Token token;
  if (this->peek(TokenType::IDENT, token)) {
    this->lexer.consume_token();
    return Ast_Identifier(token.location, std::string(token.raw_token));
  }
  return std::nullopt;
}

/* Types */

Type_Ptr Parser::parse_type() {
  auto type_name = this->parse_identifier();
  if (type_name) {
    UncheckedType type{*type_name};
    return std::make_shared<Type>(type);
  }
  return nullptr;
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
