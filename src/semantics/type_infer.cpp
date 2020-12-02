#include <mpark/patterns.hpp>
#include <boost/range/combine.hpp>

#include "ast/util.h"
#include "ast/visitor.h"
#include "ast/ast_builder.h"
#include "sema/type_infer.h"
#include "errors/compiler_errors.h"

#include <iostream>
#include <utility>
#include <optional>

namespace Infer {

using SpecialConstraints = std::vector<std::pair<TypeVar, TypeVar>>;
static std::pair<Substitution, std::optional<CompilerError>> unify(ConstraintSet&& constraints);
static bool do_not_add_reasons = false;

// subs reasoning = map tvar id -> source location, tvar id
// then when unify fails apply current subs to reasoning & track to where it first go it's concrete type

static std::map<int32_t, std::vector<std::pair<SourceLocation, Type_Ptr>>> subs_reasoning;
static Constraint failed_constraint; // for now...

static Type_Ptr substitute(
  Type_Ptr current_type, TypeVar tvar, Type_Ptr replacement,
  Substitution const & subs, SourceLocation origin
) {
  using namespace mpark::patterns;
  return match(current_type->v)(
    pattern(as<PrimativeType>(_)) = [&]{ return current_type; },
    pattern(as<LambdaType>(arg)) = [&](auto lambda_type) {
      lambda_type.return_type = substitute(
        lambda_type.return_type, tvar, replacement, subs, origin);
      for (auto& arg_type: lambda_type.argument_types) {
        arg_type = substitute(arg_type, tvar, replacement, subs, origin);
      }
      return to_type_ptr(lambda_type);
    },
    pattern(as<TupleType>(arg)) = [&](auto tuple_type) {
      for (auto& el_type: tuple_type.element_types) {
        el_type = substitute(el_type, tvar, replacement, subs, origin);
      }
      return to_type_ptr(tuple_type);
    },
    pattern(as<TypeVar>(arg)) = [&](auto const & current_tvar) {
      if (current_tvar.id == tvar.id) {
        if (auto rtvar = std::get_if<TypeVar>(&replacement->v)) {
          std::cout << "tvar replaced " << current_tvar.id << " " << rtvar->id << '\n';
          auto& reasons = subs_reasoning[rtvar->id];
          auto& pior_reasons = subs_reasoning[current_tvar.id];
          reasons.insert(reasons.end(), pior_reasons.begin(), pior_reasons.end());
          pior_reasons = reasons;
        } // } else {
        //    subs_reasoning[current_tvar.id].push_back(std::make_pair(origin, replacement));
        // }

        return replacement;
      }
      return current_type;
    },
    pattern(_) = [&]() -> Type_Ptr {
      throw UnifyError("unknown type substitution " + type_to_string(current_type.get()));
    }
  );
}

static Type_Ptr apply_type(Type_Ptr type, Substitution const & subs, SourceLocation origin) {
  auto result = type;
  for (auto& [tvar, solved_type]: subs) {
    result = substitute(result, tvar, solved_type, subs, origin);
  }
  return result;
}

static void apply_constraint(
  Constraint& c, Substitution const & subs
) {
  c.types.first = apply_type(c.types.first, subs, c.origin);
  c.types.second = apply_type(c.types.second, subs, c.origin);
}

static void apply_subs(std::vector<Constraint>& constraints, Substitution const & subs
) {
  for (auto& c: constraints) {
    apply_constraint(c, subs);
  }
}

void extract_tvars(Type_Ptr type, std::vector<TypeVar>& type_vars) {
  using namespace mpark::patterns;
  match(type->v)(
    pattern(as<LambdaType>(arg)) =
      [&](auto const & lambda) {
        extract_tvars(lambda.return_type, type_vars);
        for (auto arg_type: lambda.argument_types) {
          extract_tvars(arg_type, type_vars);
        }
      },
    pattern(as<TupleType>(arg)) =
      [&](auto const & tuple) {
        for (auto el_type: tuple.element_types) {
          extract_tvars(el_type, type_vars);
        }
      },
    pattern(as<TypeVar>(arg)) = [&](auto tvar){ type_vars.push_back(tvar); },
    pattern(_) = []{});
}

static bool occurs(TypeVar tvar, Type_Ptr type) {
  using namespace mpark::patterns;
  return match(type->v)(
    pattern(as<LambdaType>(arg)) =
      [&](auto const & lambda) {
        return occurs(tvar, lambda.return_type)
          || std::any_of(lambda.argument_types.begin(), lambda.argument_types.end(),
            [&](auto arg_type){ return occurs(tvar, arg_type); });
      },
    pattern(as<TupleType>(arg)) =
      [&](auto const & tuple) {
          return std::any_of(tuple.element_types.begin(), tuple.element_types.end(),
            [&](auto el_type){ return occurs(tvar, el_type); });
      },
    pattern(as<TypeVar>(arg)) =
      [&](auto const & current_tvar) {
        return tvar.id == TypeVar::ANY || current_tvar.id == tvar.id;
      },
    pattern(_) = []{ return false; });
}

static Substitution unify_var(TypeVar tvar, Type_Ptr type, SourceLocation origin) {
  auto type_var = std::get_if<TypeVar>(&type->v);
  if (type_var) {
    if (type_var->id == tvar.id) {
      return Substitution{};
    } else {
      if (!do_not_add_reasons) {
        std::cout << "unify: " << tvar.id << " " << type_to_string(type.get()) << '\n';
        subs_reasoning[tvar.id].push_back(std::make_pair(origin, type));
      }
      return Substitution{{tvar, type}};
    }
  } else if (occurs(tvar, type)) {
    throw UnifyError("circular type usage detected");
  } else {
    if (!do_not_add_reasons) {
      std::cout << "unify: " << tvar.id << " " << type_to_string(type.get()) << '\n';
      subs_reasoning[tvar.id].push_back(std::make_pair(origin, type));
    }
    return Substitution{{tvar, type}};
  }
}

[[ noreturn ]]
static void throw_unify_error(Constraint const & constraint) {
  failed_constraint = constraint;
  throw_compile_error(constraint.origin, constraint.error_template,
    type_to_string(constraint.types.first.get()),
    type_to_string(constraint.types.second.get()));
}

static Substitution try_unify_sub_constraints(
  Constraint const & parent,
  ConstraintSet&& sub_constraints
) {
  // Kinda a hack...
  // std::vector<int32_t> sub_reasons;
  // for (auto& c: sub_constraints) {
  //   if (c.tvar1 >= 0) {
  //     sub_reasons.push_back(c.tvar1);
  //   }
  //   if (c.tvar2 >= 0) {
  //     sub_reasons.push_back(c.tvar2);
  //   }
  // }
  do_not_add_reasons = true;
  try {
    return std::get<0>(unify(std::move(sub_constraints)));
  } catch (...) {
    // for (auto tvar: sub_reasons) {
    //   std::cout << tvar << '\n';
    //   if (parent.tvar1 >= 0) {
    //     std::cout << "here" << parent.tvar1 << ' ' << tvar << '\n';
    //     subs_reasoning[parent.tvar1].insert(subs_reasoning[parent.tvar1].end(),
    //       subs_reasoning[tvar].begin(), subs_reasoning[tvar].end());

    //   } else if (parent.tvar2 >= 0) {
    //     subs_reasoning[parent.tvar2].insert(subs_reasoning[parent.tvar2].end(),
    //       subs_reasoning[tvar].begin(), subs_reasoning[tvar].end());
    //   }

    // }
    do_not_add_reasons = false;
    throw_unify_error(parent);
  }
  do_not_add_reasons = false;
}

static Substitution unify_one(Constraint constraint) {
  using namespace mpark::patterns;
  auto [a, b] = constraint.types;
  return match(a->v, b->v)(
    pattern(as<PrimativeType>(arg), as<PrimativeType>(arg)) = [](auto& p1, auto& p2){
      WHEN(p1.tag == p2.tag) {
        return Substitution{};
      };
    },
    pattern(as<LambdaType>(arg), as<LambdaType>(arg)) = [&](auto& l1, auto& l2) {
      WHEN(l1.argument_types.size() == l2.argument_types.size()) {
        // std::cout << "ugh\n";
        ConstraintSet new_constraints;
        auto ret_constraint = constraint;
        ret_constraint.types.first = l1.return_type;
        ret_constraint.types.second = l2.return_type;
        ret_constraint.init();
        new_constraints.push_back(ret_constraint);
        for (auto type_pair: boost::combine(l1.argument_types, l2.argument_types)) {
          Constraint arg_constraint = constraint;
          boost::tie(arg_constraint.types.first, arg_constraint.types.second) = type_pair;
          arg_constraint.init();
          new_constraints.push_back(arg_constraint);
        }
        return try_unify_sub_constraints(constraint, std::move(new_constraints));
      };
    },
    pattern(as<TupleType>(arg), as<TupleType>(arg)) = [&](auto& t1, auto& t2) {
      WHEN(t1.element_types.size() == t2.element_types.size()) {
        ConstraintSet tuple_constraints;
        for (auto type_pair: boost::combine(t1.element_types, t2.element_types)) {
          Constraint tup_constraint = constraint;
          boost::tie(tup_constraint.types.first, tup_constraint.types.second) = type_pair;
          tup_constraint.init();
          tuple_constraints.push_back(tup_constraint);
        }
        return try_unify_sub_constraints(constraint, std::move(tuple_constraints));
      };
    },
    pattern(as<TypeVar>(arg), _) = [&](auto& tvar){
      return unify_var(tvar, b, constraint.origin);
    },
    pattern(_, as<TypeVar>(arg)) = [&](auto& tvar){
      return unify_var(tvar, a, constraint.origin);
    },
    pattern(_, _) = [&]() -> Substitution { throw_unify_error(constraint); }
  );
}

static void merge_left(Substitution& target, Substitution const & other) {
  // Merge 'other' into target (left)
  for (auto& [_, s]: target) {
    s = apply_type(s, other, {}); // I think it's safe to ignore the origin here
  }
  target.insert(other.begin(), other.end());
}

static bool satisfies_special_constraints(Substitution const & subs, SpecialConstraints const & constraints) {
  auto sat_constraint = [&](TypeVar::Constraint constraint, Type const & type) {
    using namespace mpark::patterns;
    return match(type.v)(
      pattern(as<PrimativeType>(arg)) = [&](auto const & primative) {
        return primative.satisfies(constraint);
      },
      pattern(_) = []{ return false; }
    );
  };

  for (auto [tvar, constraint]: constraints) {
    if (subs.contains(tvar)) {
      auto type = subs.at(tvar);
      if (!sat_constraint(static_cast<TypeVar::Constraint>(constraint.id), *type)) {
        return false;
      }
    }
  }
  return true;
}

static std::pair<Substitution, std::optional<CompilerError>> unify(ConstraintSet&& constraints) {
  if (constraints.size() == 0) {
    return std::make_pair(Substitution{}, std::nullopt);
  }

  Constraint top_constraint = constraints.back();
  constraints.pop_back();
  auto subs = unify_one(top_constraint);
  apply_subs(constraints, subs);
  try {
    auto [more_subs, e] = unify(std::move(constraints));
    merge_left(subs, more_subs);
    return std::make_pair(subs, e);
  } catch (CompilerError& e) {
    return std::make_pair(subs, e);
  }
}

static void print_subs_reasons(int32_t tvar, Substitution const & subs) {
  std::cout << tvar <<'\n';
  if (tvar >= 0 && subs_reasoning.contains(tvar)) {
    // std::cout << "T" << tvar << " reasons:\n";
    for (auto const & [loc, type]: subs_reasoning.at(tvar)) {
      // std::cout << loc.start_line << ':' << loc.start_column << " -> " << loc.end_line << ':' << loc.end_column << '\n';
      if (!type) continue;
      auto error_type = apply_type(type, subs, loc);
      if (std::holds_alternative<TypeVar>(error_type->v)) {
        continue; // not informative
      }
      hack_backtrack_infer->push_back(CompilerMessage{loc, "must be " + type_to_string(error_type.get()), CompilerMessage::NOTE});
    }
    // std::cout << '\n';
  }
}

Substitution unify_and_apply(ConstraintSet && constraints) {
  SpecialConstraints special_constraints;

  // Collect special constraints
  auto res = std::remove_if(constraints.begin(), constraints.end(), [&](auto& constraint) {
    using namespace mpark::patterns;
    constraint.init();
    return match(constraint.types.first->v, constraint.types.second->v)(
      pattern(as<TypeVar>(arg), as<TypeVar>(arg)) = [&](auto t1, auto t2) {
        WHEN(t2.special()) {
          special_constraints.push_back(std::make_pair(t1, t2));
          return true; // remove
        };
      },
      pattern(_,_) = [&]{ return false; }
    );
  });

  constraints.erase(res, constraints.end());

  // Since we want pop_back to return the first contraint so the errors make sense
  std::reverse(constraints.begin(), constraints.end());


  auto [subs, error] = unify(std::move(constraints));

  if (error) {
    // for (auto c: failed_constraint) {

    for (auto& tvar: failed_constraint.contained_tvars) {
      if (tvar.id < 0) continue;
      print_subs_reasons(tvar.id, subs);
      // print_subs_reasons(failed_constraint.tvar2, subs);
    }
    subs_reasoning = {}; // proto hack
    // }
    // failed_constraint.types.first=  apply_type(failed_constraint.types.first, subs, failed_constraint.origin);
    // failed_constraint.types.second=  apply_type(failed_constraint.types.second, subs, failed_constraint.origin);
    // throw_unify_error(failed_constraint);
    throw *error;

  }

  if (!satisfies_special_constraints(subs, special_constraints)) {
    throw UnifyError("additional type constraints not met");
  }

  for (auto& [tvar, sub]: subs) {
    if (auto target = tvar.substitute.lock()) {
      if (occurs(TypeVar(TypeVar::ANY), sub)) {
        throw UnifyError("incomplete substitution");
      }
      assert(sub != nullptr && "fix me! void not supported in infer");
      *target = *sub;
    }
  }
  return subs;
}

void generate_call_constraints(
  Type_Ptr& callee_type, Ast_Call& call, ConstraintSet& constraints
) {
  if (std::holds_alternative<TypeVar>(callee_type->v)) {
    LambdaType call_type;
    call_type.return_type = to_type_ptr(TypeVar());
    std::generate_n(std::back_inserter(call_type.argument_types),
      call.arguments.size(), []{ return to_type_ptr(TypeVar()); });
    auto call_type_ptr = to_type_ptr(call_type);
    auto constraint = Constraint(
      AstHelper::extract_location(call.callee), callee_type, call_type_ptr);
    constraint.error_template = "call requires [{1}] but callee is [{0}]";
    constraints.push_back(constraint);
    callee_type = call.get_meta().owned_type = call_type_ptr;
  }
}

void generate_array_index_constraints(
  Type_Ptr& array_type, Ast_Index_Access& index, ConstraintSet& constraints
) {
  // TODO
}

void generate_tuple_assign_constraints(
  Ast_Assign& tuple_assign, ConstraintSet& constraints
) {
  auto tuple_type = extract_type(tuple_assign.expression->meta.type);
  if (std::holds_alternative<TypeVar>(tuple_type->v)) {
    TupleType assign_type;
    std::generate_n(std::back_inserter(assign_type.element_types),
      std::get<Ast_Tuple_Literal>(tuple_assign.target->v).elements.size(),
      []{ return to_type_ptr(TypeVar()); });
    auto assign_type_ptr = to_type_ptr(assign_type);
    constraints.push_back(
      Constraint(AstHelper::extract_location(tuple_assign.expression),
        tuple_type, assign_type_ptr));
    tuple_assign.expression->meta.type
      = tuple_assign.expression->meta.owned_type
      = assign_type_ptr;
  }
}

bool generate_tuple_destructure_constraints(
  TupleBinding const & bindings, Type_Ptr& init_type, ConstraintSet& constraints,
  SourceLocation loc
) {
  // Almost the same as assign
  if (std::holds_alternative<TypeVar>(init_type->v)) {
    TupleType binding_type;
    std::generate_n(std::back_inserter(binding_type.element_types), bindings.binds.size(),
      []{ return to_type_ptr(TypeVar()); });
    auto binding_type_ptr = to_type_ptr(binding_type);
    // FIXME: Add loc
    auto constraint = Constraint(loc, init_type, binding_type_ptr);
    constraint.error_template = "binding type {} does not match init type {}";
    constraints.push_back(constraint);
    init_type = binding_type_ptr;
    return true;
  }
  return false;
}

}
