#pragma once

#include <stack>
#include <vector>
#include <formatxx/std_string.h>

#include "sema/types.h"
#include "sema/pod_info.h"
#include "sema/type_infer.h"

#include "ast/ast.h"
#include "errors/compiler_message.h"

struct Semantics {
  using CompilerWarnings = std::vector<CompilerMessage>;

  Semantics();

  void analyse_file(Ast_File& file);

  inline void set_source(Lexer& lexer) {
    this->source_file = &lexer;
  }

  CompilerWarnings const & get_warnings() { return warnings; }

  inline void disable_type_inference_for_testing() {
    // Sema must only be run on one function or this will create bugs
    this->disable_type_infer = true;
  }

  inline Infer& get_infer_for_testing() { return *infer; }
private:
  // FIXME: These both should be refs... but I don't have the ctor...
  AstContext* ctx;
  std::optional<AstBuilder> builder;
  std::optional<Infer> infer;

  Lexer* source_file = nullptr;

  ResolvedPodInfoMap resolved_pods;

  // Only used for testing allows to seperate sema + infer
  bool disable_type_infer = false;

  // Simply because it's a pain to pass this around (top of stack == current function)
  std::stack<Type_Ptr> expected_returns;

  std::stack<Stmt_Ptr> active_loops;
  inline void enter_loop(Stmt_Ptr loop) { active_loops.push(loop); }
  inline void exit_loop() { active_loops.pop(); }
  inline Stmt_Ptr in_loop() {
    if (!active_loops.empty()) {
      return active_loops.top();
    }
    return nullptr;
  }

  bool assert_valid_binding(
    Ast_Identifier const& lvalue,
    SourceLocation bind_location,
    Type_Ptr type,
    Type_Ptr to_bind,
    Expr_Ptr const expression,
    Infer::ConstraintOrigin = std::nullopt);

  bool assert_valid_binding(
    Ast_Identifier const & lvalue,
    Type_Ptr type,
    Expr_Ptr const expression);

  Symbol* emit_warning_if_shadows(
    Ast_Identifier& ident, Scope& scope, std::string warning);

  void analyse_pod(Ast_Pod_Declaration& pod, Scope& scope);
  void analyse_function_header(Ast_Function_Declaration& func);
  Type_Ptr analyse_function_body(Ast_Function_Declaration& func);
  void analyse_assignment(Ast_Assign& assign, Scope& scope);
  void analyse_statement(Ast_Statement& stmt, Scope& scope);
  void analyse_expression_statement(Ast_Expression_Statement& expr_stmt, Scope& scope);
  void analyse_for_loop(Ast_For_Loop& for_loop, Scope& scope);

  void check_tuple_bindings(
    Ast_Tuple_Binds& bindings, Expr_Ptr init, Type_Ptr init_type, Scope& scope);
  void check_pod_bindings(
    Ast_Pod_Binds& bindings, Expr_Ptr init, Type_Ptr init_type, Scope& scope);
  void analyse_binding_decl(Ast_Structural_Binding& binding, Scope& scope);

  Type_Ptr analyse_block(Ast_Block& block, Scope& scope);
  Type_Ptr analyse_expression(Ast_Expression& expr, Scope& scope);
  Type_Ptr analyse_unary_expression(Ast_Unary_Operation& unary, Scope& scope);
  Type_Ptr analyse_binary_expression(Ast_Binary_Operation& expr, Scope& scope);
  Type_Ptr analyse_call(Ast_Call& expr, Scope& scope);
  Type_Ptr analyse_as_cast(Ast_As_Cast& as_cast, Scope& scope);

  void expand_macro_expression(Ast_Expression& target, Ast_Call& macro_call, Scope& scope);

  Ast_Lambda builtin_bind(Ast_Call& bind_call, Scope& scope);

#include "sema_warnings.h"

  CompilerWarnings warnings;
};
