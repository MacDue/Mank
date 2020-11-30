
#include "sema/types.h"
#include "sema/sema_errors.h"
#include "binding_helpers.h"

/*
  A binding is when a variable is first initialized.
  e.g. when a variable is declared or a function is called (and expressions are bound to it's params)
*/

bool assert_valid_binding(
  Ast_Identifier const& lvalue,
  SourceLocation bind_location,
  Type_Ptr type,
  Type_Ptr to_bind,
  Ast_Expression const * expression,
  Infer::ConstraintSet* constraints
) {
  if (expression) {
    if (!match_types(type, to_bind, constraints)) {
      throw_compile_error(bind_location, "cannot bind expression with type {} to {}",
        type_to_string(to_bind.get()), type_to_string(type.get()));
    }
  }

  if (is_reference_type(type.get())) {
    if (!to_bind) {
      throw_sema_error_at(lvalue, "reference must be initialized");
    } else if (!expression->is_lvalue()) {
      // References can only be bound to lvalues (that have addresses)
      throw_sema_error_at(*expression, "cannot bind rvalue to lvalue reference");
    }
  }

  return true;
}

bool assert_valid_binding(
  Ast_Identifier const & lvalue,
  Type_Ptr type,
  Ast_Expression const * expression,
  Infer::ConstraintSet* constraints
) {
  return assert_valid_binding(lvalue,
    expression
      ? std::visit([](auto const & ast){ return ast.location; }, expression->v)
      : SourceLocation{}, type,
    expression
      ? extract_type_nullable(expression->meta.type)
      : Type_Ptr(nullptr),
    expression, constraints);
}
