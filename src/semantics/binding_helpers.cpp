
#include "sema/types.h"
#include "sema/sema_errors.h"
#include "binding_helpers.h"

/*
  A binding is when a variable is first initialized.
  e.g. when a variable is declared or a function is called (and expressions are bound to it's params)
*/
bool assert_valid_binding(
  Ast_Identifier const & lvalue,
  Type const * type,
  Ast_Expression const * expression
) {
  // If there's an expression it must match the type.
  if (expression) {
    auto expression_type = extract_type_nullable(expression->meta.type);
    if (!match_types(type, expression_type.get())) {
      throw_sema_error_at(*expression, "cannot bind expression with type {} to {}",
        type_to_string(expression_type.get()), type_to_string(type));
    }
  }

  if (is_reference_type(type)) {
    if (!expression) {
      throw_sema_error_at(lvalue, "reference must be initialized");
    } else if (!expression->is_lvalue()) {
      // References can only be bound to lvalues (that have addresses)
      throw_sema_error_at(*expression, "cannot bind rvalue to lvalue reference");
    }
  }

  // Otherwise if the target is an lvalue it's a valid binding
  return true;
}
