#pragma once

#include <optional>

#include "ast/ast.h"
#include "parser/lexer.h"
#include "errors/compiler_errors.h"

SourceLocation join_source_locations(SourceLocation start, SourceLocation end);

struct Parser {
  Parser(Lexer& lexer)
    : lexer{lexer} {}

  AstContext* ctx = nullptr;

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
  auto& mark_ast_location(SourceLocation start, AstPtr<T>& ast) {
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
  Type_Ptr parse_type(bool default_tvar = false);
  Type_Ptr parse_base_type(bool default_tvar = false);
  Type_Ptr parse_array_type(Type_Ptr base_type);
  Type_Ptr parse_list_type(Type_Ptr base_type);
  std::vector<Type_Ptr> parse_type_list(TokenType left_delim, TokenType right_delim);
  Type_Ptr parse_lambda_type();
  Type_Ptr parse_pod();
  Type_Ptr parse_tuple_type();
  Type_Ptr parse_function();

  /* Constructs */
  std::vector<Ast_Argument> parse_arguments(
    TokenType left_delim = TokenType::LEFT_PAREN,
    TokenType right_delim = TokenType::RIGHT_PAREN,
    bool insert_tvars = false);
  std::optional<Ast_Identifier> parse_identifier();
  std::optional<Ast_Block> parse_block();

  /* Statements */
  Stmt_Ptr parse_statement();
  Stmt_Ptr parse_assign(Expr_Ptr lhs);
  Stmt_Ptr parse_for_loop();
  Stmt_Ptr parse_loop();
  Stmt_Ptr parse_while_loop();
  Stmt_Ptr parse_const_decl();

  // void parse_parse_tuple_bindings(bindings&)
  // void parse_pod_bindings(bindings&)

  Ast_Binding parse_binding();
  Ast_Pod_Binds parse_pod_binds();
  Ast_Tuple_Binds parse_tuple_binds();
  Stmt_Ptr parse_structural_binding();

  /* Expressions */
  std::vector<Expr_Ptr> parse_expression_list(
    TokenType left_delim = TokenType::LEFT_PAREN,
    TokenType right_delim = TokenType::RIGHT_PAREN);
  Expr_Ptr parse_expression(bool brace_delimited = false);
  Expr_Ptr parse_postfix_expression(bool brace_delimited);
  Expr_Ptr parse_call(Expr_Ptr target);
  Expr_Ptr parse_field_access(Expr_Ptr object);
  Expr_Ptr parse_index_access(Expr_Ptr object);
  Expr_Ptr parse_primary_expression(bool brace_delimited);
  Expr_Ptr parse_parenthesised_expression();
  Expr_Ptr parse_if();
  Expr_Ptr parse_binary_expression(bool brace_delimited);
  Expr_Ptr parse_unary(bool brace_delimited);
  Expr_Ptr parse_literal();
  Expr_Ptr parse_array_literal();
  Expr_Ptr parse_lambda();
  Expr_Ptr parse_tuple_literal(Expr_Ptr first_element);
  Expr_Ptr parse_pod_literal(
    Ast_Identifier pod_name, std::vector<Type_Ptr> specializations = {});

  /* Simple helpers */
  bool consume(TokenType token_type);
  void expect(TokenType token_type);
  bool peek(TokenType token_type);
  bool peek(TokenType token_type, Token& token);

  inline bool was_previously_terminating_symbol() {
    return lexer.get_last_consumed().type == TokenType::RIGHT_BRACE;
  }
};
