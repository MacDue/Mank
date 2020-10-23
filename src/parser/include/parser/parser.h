#pragma once

#include <optional>
#include <iostream>

#include "ast/ast.h"
#include "parser/lexer.h"
#include "errors/compiler_errors.h"

SourceLocation join_source_locations(SourceLocation start, SourceLocation end);

struct Parser {
  Parser(Lexer& lexer)
    : lexer{lexer} {}

  Ast_File parse_file();

  static Ast_File parse_from_file(std::string file_path);
  static Ast_File parse_from_string(std::string source);
private:
  Lexer& lexer;

  /* Location Helpers */
  SourceLocation current_location() {
    return this->lexer.peek_next_token_location();
  }

  template<typename T>
  void mark_ast_location(SourceLocation start, T& ast) {
    ast.location = join_source_locations(start, lexer.get_last_consumed_location());
  }

  template<typename T>
  auto& mark_ast_location(SourceLocation start, std::shared_ptr<T>& ast) {
    std::visit([&](auto & ast){
      mark_ast_location(start, ast);
    }, ast->v);
    return ast;
  }

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
  Type_Ptr parse_pod();
  Function_Ptr parse_function();

  /* Constructs */
  std::vector<Ast_Argument> parse_arguments(
    TokenType left_delim = TokenType::LEFT_PAREN,
    TokenType right_delim = TokenType::RIGHT_PAREN);
  std::optional<Ast_Identifier> parse_identifier();
  std::optional<Ast_Block> parse_block();

  /* Statements */
  Statement_Ptr parse_statement();
  Statement_Ptr parse_for_loop();

  /* Expressions */
  Expression_Ptr parse_expression();
  Expression_Ptr parse_postfix_expression();
  Expression_Ptr parse_call(Expression_Ptr target);
  Expression_Ptr parse_primary_expression();
  Expression_Ptr parse_parenthesised_expression();
  Expression_Ptr parse_if();
  Expression_Ptr parse_binary_expression();
  Expression_Ptr parse_unary();
  Expression_Ptr parse_literal();

  /* Simple helpers */
  bool consume(TokenType token_type);
  void expect(TokenType token_type);
  bool peek(TokenType token_type);
  bool peek(TokenType token_type, Token& token);

  inline bool was_previously_terminating_symbol() {
    return lexer.get_last_consumed().type == TokenType::RIGHT_BRACE;
  }
};
