#pragma once

#include "ast/expr.h"

bool assert_valid_binding(
  Ast_Identifier const& lvalue,
  SourceLocation bind_location,
  Type_Ptr type,
  Type_Ptr to_bind,
  Ast_Expression const * expression,
  Infer::ConstraintSet* constraints
);

bool assert_valid_binding(
  Ast_Identifier const & lvalue,
  Type_Ptr type,
  Ast_Expression const * expression,
  Infer::ConstraintSet* constraints
);
