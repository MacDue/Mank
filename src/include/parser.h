#pragma once

#include <optional>

#include "ast.h"
#include "lexer.h"

struct Parser {
  Parser(Lexer& lexer)
    : lexer{lexer} {}

  Ast_File parse_file();

private:
  Lexer& lexer;

  /* Types */
  std::shared_ptr<Type> parse_type();
  std::shared_ptr<Ast_Function_Declaration> parse_function();

  /* Constructs */
  std::optional<Ast_Identifier> parse_identifier();
  std::optional<Ast_Block> parse_block();

  /* Statements */
  Statement_Ptr parse_statement();
  Statement_Ptr parse_if();

  /* Expressions */
  Expression_Ptr parse_expression();
  Expression_Ptr parse_postfix_expression();
  Expression_Ptr parse_call(Expression_Ptr target);
  Expression_Ptr parse_primary_expression();
  Expression_Ptr parse_parenthesised_expression();
  Expression_Ptr parse_binary_expression();
  Expression_Ptr parse_unary();
  Expression_Ptr parse_literal();

  /* Simple helpers */
  bool consume(TokenType token_type);
  void expect(TokenType token_type);
  bool peek(TokenType token_type);
  bool peek(TokenType token_type, Token& token);
};
