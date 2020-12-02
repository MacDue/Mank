#pragma once

#include <set>
#include <map>
#include <utility>
#include <stdexcept>

#include "ast/types.h"
#include "errors/compiler_message.h"
#include <iostream>
namespace Infer {

inline std::vector<CompilerMessage>* hack_backtrack_infer = nullptr;
void extract_tvars(Type_Ptr type, std::set<TypeVar>& type_vars);


struct Constraint {
  SourceLocation origin;
  std::pair<Type_Ptr, Type_Ptr> types;
  // int32_t tvar1 = -1, tvar2 = -1;
  std::set<TypeVar> contained_tvars;
  char const * error_template = "DEBUG: infer failed a = {}, b = {}";

  Constraint() { origin = {}; }
  Constraint(SourceLocation loc, Type_Ptr t1, Type_Ptr t2)
    : origin{loc}, types{std::make_pair(t1, t2)} {}

  inline void init() {
    // if (auto tvar1 = std::get_if<TypeVar>(&types.first->v)) {
    //   this->tvar1 = tvar1->id;
    // }
    // if (auto tvar2 = std::get_if<TypeVar>(&types.second->v)) {
    //   this->tvar2 = tvar2->id;
    // }
    // std::cout << "init: " << tvar1 << ", " << tvar2 << '\n';
    extract_tvars(types.first, contained_tvars);
    extract_tvars(types.second, contained_tvars);
  }
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

std::optional<Constraint> generate_tuple_destructure_constraints(
  TupleBinding const & bindings, Type_Ptr& init_type, SourceLocation loc);

}
