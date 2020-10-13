#pragma once

#include <vector>
#include <formatxx/std_string.h>

#include "ast.h"
#include "compiler_message.h"

struct Semantics {
  using CompilerWarnings = std::vector<CompilerMessage>;

  void analyse_file(Ast_File& file);

  CompilerWarnings const & get_warnings() { return warnings; }
private:
  void analyse_function_header(Ast_Function_Declaration& func);
  void analyse_function_body(Ast_Function_Declaration& func);
  void analyse_statement(Ast_Statement& stmt, Scope& scope, Type* return_type = nullptr);
  void analyse_block(Ast_Block& block, Scope& scope, Type* return_type);
  void analyse_expression_statement(Ast_Expression_Statement& expr_stmt, Scope& scope);

  Type_Ptr analyse_expression(Ast_Expression& expr, Scope& scope);
  Type_Ptr analyse_unary_expression(Ast_Unary_Operation& unary, Scope& scope);
  Type_Ptr analyse_binary_expression(Ast_Binary_Operation& expr, Scope& scope);
  Type_Ptr analyse_call(Ast_Call& expr, Scope& scope);

#include "sema_warnings.h"

  CompilerWarnings warnings;
};
