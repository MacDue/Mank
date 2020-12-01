#pragma once

#include <set>
#include <map>
#include <utility>
#include <stdexcept>

#include "ast/types.h"

namespace Infer {

struct Constraint {
  SourceLocation origin;
  std::pair<Type_Ptr, Type_Ptr> types;
  char const * error_template = "infer failed {} {}";

  Constraint() { origin = {}; }
  Constraint(SourceLocation loc, Type_Ptr t1, Type_Ptr t2)
    : origin{loc}, types{std::make_pair(t1, t2)} {}
};

using ConstraintSet = std::vector<Constraint>;
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
