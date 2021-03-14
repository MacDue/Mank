
#include "ast/util.h"
#include "sema/types.h"
#include "sema/semantics.h"
#include "errors/compiler_errors.h"

/*
  A binding is when a variable is first initialized.
  e.g. when a variable is declared or a function is called (and expressions are bound to it's params)
*/

bool Semantics::assert_valid_binding(
  Ast_Identifier const& lvalue,
  SourceLocation bind_location,
  Type_Ptr type,
  Type_Ptr to_bind,
  Expr_Ptr const expression,
  Infer::ConstraintOrigin note_spot
) {
  if (expression) {
    infer->match_or_constrain_types_at(bind_location, type, to_bind,
      "cannot bind expression with type {1} to {0}", note_spot);
  }

  if (is_reference_type(type)) {
    if (!to_bind) {
      throw_error_at(lvalue, "reference must be initialized");
    }
    infer->assert_lvalue(expression, "cannot bind rvalue to lvalue reference");
  }

  return true;
}

bool Semantics::assert_valid_binding(
  Ast_Identifier const & lvalue,
  Type_Ptr type,
  Expr_Ptr const expression
) {
  return assert_valid_binding(lvalue,
    expression
      ? AstHelper::extract_location(*expression)
      : SourceLocation{}, type,
    expression
      ? expression->meta.type
      : Type_Ptr(nullptr),
    expression);
}
