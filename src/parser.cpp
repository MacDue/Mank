#include <mpark/patterns.hpp>

#include "parser.h"
#include "compiler_errors.h"

/* Top level constructs */

Ast_File Parser::parse_file() {
  Ast_File parsed_file;
  parsed_file.filename = this->lexer.input_source_name();
  Token next_token;
  while (!this->peek(TokenType::LEX_EOF, next_token)) {
    if (next_token.type == TokenType::FUNCTION
      || next_token.type == TokenType::PROCEDURE
    ) {
      parsed_file.functions.emplace_back(this->parse_function());
    } else {
      /* TODO errors */
      throw_compile_error({}, "TODO");
    }
  }
  return parsed_file;
}

std::shared_ptr<Ast_Function_Declaration> Parser::parse_function() {
  auto parsed_function = std::make_shared<Ast_Function_Declaration>();
  if (consume(TokenType::FUNCTION) || (parsed_function->procedure = consume(TokenType::PROCEDURE))) {

    auto ident = this->parse_identifier();
    if (!ident) {
      throw_compile_error({}, "TODO");
    }
    parsed_function->identifer = *ident;
    if (!parsed_function->procedure) {
      /* TODO: functions -- return type */
      throw_compile_error({}, "TODO");
    }

    /* Parameters */
    if (consume(TokenType::LEFT_PAREN)) {
      throw_compile_error({}, "TODO");
    }

    auto body = this->parse_block();
    if (!body) {
      throw_compile_error({}, "TODO");
    }
    parsed_function->body = *body;
    return parsed_function;
  } else {
    assert(false && "should be unreachable");
  }
}

std::optional<Ast_Block> Parser::parse_block() {
  Ast_Block parsed_block;
  if (consume(TokenType::LEFT_BRACE)) {
    while (!peek(TokenType::RIGHT_BRACE)) {
      parsed_block.statements.emplace_back(this->parse_statement());
    }
    expect(TokenType::RIGHT_BRACE);
  } else {
    return std::nullopt;
  }
  return parsed_block;
}

Statement_Ptr Parser::parse_statement() {
  if (peek(TokenType::IF)) {
    return this->parse_if();
  } else if (consume(TokenType::RETURN)) {
    Ast_Return_Statement return_stmt;
    auto expr = this->parse_expression();
    return_stmt.expression = expr;
    expect(TokenType::SEMICOLON);
    return std::make_shared<Ast_Statement>(return_stmt);
  } else if (auto expr = this->parse_expression()) {
    Ast_Expression_Statement expr_stmt;
    expr_stmt.expression = expr;
    expect(TokenType::SEMICOLON);
    return std::make_shared<Ast_Statement>(expr_stmt);
  } else {
    throw_compile_error({}, "TODO");
  }
}

Statement_Ptr Parser::parse_if() {
  assert(false && "TODO");
}

/* Expressions */

Expression_Ptr Parser::parse_expression() {
  return this->parse_binary_expression();
}

Expression_Ptr Parser::parse_postfix_expression() {
  auto expr = this->parse_primary_expression();
  while (true) {
    if (peek(TokenType::LEFT_PAREN)) {
      expr = this->parse_call(std::move(expr));
    } else {
      break;
    }
  }
  return expr;
}

Expression_Ptr Parser::parse_call(Expression_Ptr target) {
  expect(TokenType::LEFT_PAREN);
  Ast_Call parsed_call;
  parsed_call.callee = target;
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
  if (peek(TokenType::LITERAL)) {
    return this->parse_literal();
  } else if (peek(TokenType::IDENT)) {
    return std::make_shared<Ast_Expression>(*this->parse_identifier());
  } else if (peek(TokenType::LEFT_PAREN)) {
    return this->parse_parenthesised_expression();
  } else {
    throw_compile_error({}, "TODO");
  }
}

Expression_Ptr Parser::parse_parenthesised_expression() {
  using namespace mpark::patterns;
  expect(TokenType::LEFT_PAREN);
  auto expr = this->parse_expression();
  expect(TokenType::RIGHT_PAREN);
  match(expr->v)(pattern(as<Ast_Binary_Operation>(arg))
    = [](auto & binary_op) {
      binary_op.parenthesised = true;
    });
  return expr;
}

/* TODO: Consider replacing with pratt parser */

static int get_binary_precdence(Ast_Operator op) {
  switch (op)
  {
    case Ast_Operator::TIMES:
    case Ast_Operator::DIVIDE:
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
        && get_binary_precdence(rhs_binop.operation) < get_binary_precdence(op)
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
      // new_binop->pos = join_pos(lhs->pos, rhs->pos);
      new_binop.left = std::move(lhs);
      new_binop.right = std::move(rhs);
      new_binop.operation = op;
      return std::make_shared<Ast_Expression>(new_binop);
    });
}

Expression_Ptr Parser::parse_binary_expression() {
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
  auto unary_op = this->lexer.peek_next_token().type;
  if (!is_unary_op(unary_op)) {
    return this->parse_postfix_expression();
  }
  Ast_Unary_Operation parsed_unary;
  parsed_unary.operation = static_cast<Ast_Operator>(unary_op);
  this->lexer.consume_token();
  // For nested unary expressions e.g. -------------10 (if you want that?)
  parsed_unary.operand = this->parse_unary();
  return std::make_shared<Ast_Expression>(parsed_unary);
}

Expression_Ptr Parser::parse_literal() {
  auto& token = this->lexer.peek_next_token();
  Ast_Literal parsed_literal;

  parsed_literal.literal_type = token.literal_type;
  if (token.literal_type == LiteralType::STRING) {
    // Remove ""s
    parsed_literal.value =
      token.raw_token.substr(1, token.raw_token.length() - 2);
  } else {
    parsed_literal.value = token.raw_token;
  }
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
    throw_compile_error({}, "TODO");
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

