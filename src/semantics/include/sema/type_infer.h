#pragma once

#include <set>
#include <map>
#include <utility>
#include <stdexcept>

#include "ast/types.h"

namespace Infer {

// struct Constraint {
//   std::pair<Type_Ptr, Type_Ptr> types;
//   SourceLocation origin;
// };

using Constraint = std::pair<Type_Ptr, Type_Ptr>;
using ConstraintSet = std::set<Constraint>;
using Substitution = std::map<TypeVar, Type_Ptr>;

class UnifyError: public std::exception {
  std::string error_message;
public:
  UnifyError(std::string error_message)
    : error_message{error_message} {}

  char const * what() const noexcept override {
    return error_message.c_str();
  }
};

Substitution unify_and_apply(ConstraintSet && constraints);

void generate_call_constraints(
  Type_Ptr& callee_type, Ast_Call& call, ConstraintSet& constraints);

void generate_tuple_assign_constraints(
  Ast_Assign& tuple_assign, ConstraintSet& constraints);

void generate_tuple_destructure_constraints(
  TupleBinding const & bindings, Type_Ptr& init_type, ConstraintSet& constraints);

}
