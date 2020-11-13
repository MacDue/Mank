#pragma once

#include <stack>
#include <vector>
#include <formatxx/std_string.h>

#include "ast/ast.h"
#include "errors/compiler_message.h"

struct Semantics {
  using CompilerWarnings = std::vector<CompilerMessage>;

  Semantics();

  void analyse_file(Ast_File& file);

  CompilerWarnings const & get_warnings() { return warnings; }
private:
  // Simply because it's a pain to pass this around (top of stack == current function)
  std::stack<Type*> expected_returns;

  Symbol* emit_warning_if_shadows(
    Ast_Identifier& ident, Scope& scope, std::string warning);

  void analyse_pod(Ast_Pod_Declaration& pod, Scope& scope);
  void analyse_function_header(Ast_Function_Declaration& func);
  void analyse_function_body(Ast_Function_Declaration& func);
  void analyse_assignment(Ast_Assign& assign, Scope& scope);
  void analyse_statement(Ast_Statement& stmt, Scope& scope);
  void analyse_expression_statement(Ast_Expression_Statement& expr_stmt, Scope& scope);
  void analyse_for_loop(Ast_For_Loop& for_loop, Scope& scope);

  Type_Ptr analyse_block(Ast_Block& block, Scope& scope);
  Type_Ptr analyse_expression(Ast_Expression& expr, Scope& scope);
  Type_Ptr analyse_unary_expression(Ast_Unary_Operation& unary, Scope& scope);
  Type_Ptr analyse_binary_expression(Ast_Binary_Operation& expr, Scope& scope);
  Type_Ptr analyse_call(Ast_Call& expr, Scope& scope);

  void expand_macro_expression(Ast_Expression& target, Ast_Call& macro_call, Scope& scope);

  Ast_Lambda builtin_bind(Ast_Call& bind_call, Scope& scope);

#include "sema_warnings.h"

  CompilerWarnings warnings;
};
