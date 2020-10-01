#pragma once

#include <optional>

#include "ast.h"
#include "lexer.h"
#include "compiler_errors.h"

struct Parser {
  Parser(Lexer& lexer)
    : lexer{lexer} {}

  Ast_File parse_file();

private:
  Lexer& lexer;

  /* Errors */
  template<typename TPattern, typename... TArgs>
  [[ noreturn ]] void throw_error_here(
    TPattern format_pattern, TArgs const & ... args
  ) {
    Token& last_token = lexer.peek_next_token();
    throw_compile_error(last_token.location, format_pattern,
      std::string(last_token.raw_token), args...);
  }

  /* Types */
  Type_Ptr parse_type();
  Function_Ptr parse_function();

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
