#pragma once

#include <stack>
#include <vector>
#include <formatxx/std_string.h>

#include "sema/types.h"
#include "sema/type_infer.h"

#include "ast/ast.h"
#include "ast/util.h"
#include "errors/compiler_message.h"

struct Semantics {
  using CompilerWarnings = std::vector<CompilerMessage>;

  Semantics();

  void analyse_file(Ast_File& file);

  CompilerWarnings const & get_warnings() { return warnings; }

  inline void disable_type_inference_for_testing() {
    // Sema must only be run on one function or this will create bugs
    this->disable_type_infer = true;
  }
  inline Infer::ConstraintSet get_constraint_test_for_testing() { return type_constraints; }
private:
  AstContext* ctx;

  // Only used for testing allows to seperate sema + infer
  bool disable_type_infer = false;

  // Simply because it's a pain to pass this around (top of stack == current function)
  std::stack<Type_Ptr> expected_returns;

  Infer::ConstraintSet type_constraints;
  inline void reset_type_constraints() { type_constraints = {}; }

  bool match_or_constrain_types_at(
    SourceLocation loc, Type_Ptr t1, Type_Ptr t2, char const * error_template);

  template<typename TAst>
  inline bool match_or_constrain_types_at(
    TAst& ast, Type_Ptr t1, Type_Ptr t2, char const * error_template
  ) {
    return match_or_constrain_types_at(
      AstHelper::extract_location(ast), t1, t2, error_template);
  }

  bool assert_valid_binding(
    Ast_Identifier const& lvalue,
    SourceLocation bind_location,
    Type_Ptr type,
    Type_Ptr to_bind,
    Ast_Expression const * expression,
    bool match_tvars = true);

  bool assert_valid_binding(
    Ast_Identifier const & lvalue,
    Type_Ptr type,
    Ast_Expression const * expression);

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
    TupleBinding& bindings, Ast_Expression& init, Type_Ptr& init_type, Scope& scope,
    bool to_infer = false);
  void analyse_tuple_binding_decl(Ast_Tuple_Structural_Binding& binding, Scope& scope);

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
