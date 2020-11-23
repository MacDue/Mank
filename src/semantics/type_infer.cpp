#include <mpark/patterns.hpp>
#include <boost/range/combine.hpp>

#include "ast/visitor.h"
#include "ast/ast_builder.h"
#include "sema/type_infer.h"

namespace Infer {

using SpecialConstraints = std::vector<std::pair<TypeVar, TypeVar>>;
static Substitution unify(ConstraintSet&& constraints);

static Type_Ptr substitute(
  Type_Ptr current_type, TypeVar tvar, Type_Ptr replacement,
  Substitution const & subs
) {
  using namespace mpark::patterns;
  return match(current_type->v)(
    pattern(as<PrimativeType>(_)) = [&]{ return current_type; },
    pattern(as<LambdaType>(arg)) = [&](auto lambda_type) {
      lambda_type.return_type = substitute(
        lambda_type.return_type, tvar, replacement, subs);
      for (auto& arg_type: lambda_type.argument_types) {
        arg_type = substitute(arg_type, tvar, replacement, subs);
      }
      return to_type_ptr(lambda_type);
    },
    pattern(as<TypeVar>(arg)) = [&](auto const & current_tvar) {
      if (current_tvar.id == tvar.id) {
        return replacement;
      }
      return current_type;
    },
    pattern(_) = []() -> Type_Ptr {
      throw UnifyError("unknown type substitution");
    }
  );
}

static Type_Ptr apply_type(Type_Ptr type, Substitution const & subs) {
  auto result = type;
  for (auto& [tvar, solved_type]: subs) {
    result = substitute(result, tvar, solved_type, subs);
  }
  return result;
}

static void apply_constraint(
  Constraint& c, Substitution const & subs
) {
  c.first = apply_type(c.first, subs);
  c.second = apply_type(c.second, subs);
}

static void apply_subs(std::vector<Constraint>& constraints, Substitution const & subs
) {
  for (auto& c: constraints) {
    apply_constraint(c, subs);
  }
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
    pattern(as<TypeVar>(arg)) =
      [&](auto const & current_tvar) {
        return tvar.id == TypeVar::ANY || current_tvar.id == tvar.id;
      },
    pattern(_) = []{ return false; });
}

static Substitution unify_var(TypeVar tvar, Type_Ptr type) {
  auto type_var = std::get_if<TypeVar>(&type->v);
  if (type_var) {
    if (type_var->id == tvar.id) {
      return Substitution{};
    } else {
      return Substitution{{tvar, type}};
    }
  } else if (occurs(tvar, type)) {
    throw UnifyError("circular type usage detected");
  } else {
    return Substitution{{tvar, type}};
  }
}

static Substitution unify_one(Constraint constraint) {
  using namespace mpark::patterns;
  auto [a, b] = constraint;
  return match(a->v, b->v)(
    pattern(as<PrimativeType>(arg), as<PrimativeType>(arg)) = [](auto& p1, auto& p2){
      WHEN(p1.tag == p2.tag) {
        return Substitution{};
      };
    },
    pattern(as<LambdaType>(arg), as<LambdaType>(arg)) = [](auto& l1, auto& l2) {
      WHEN(l1.argument_types.size() == l2.argument_types.size()) {
        ConstraintSet new_constraints;
        new_constraints.insert(Constraint(l1.return_type, l2.return_type));
        for (auto type_pair: boost::combine(l1.argument_types, l2.argument_types)) {
          Constraint arg_constraint;
          boost::tie(arg_constraint.first, arg_constraint.second) = type_pair;
          new_constraints.insert(arg_constraint);
        }
        return unify(std::move(new_constraints));
      };
    },
    pattern(as<TypeVar>(arg), _) = [&](auto& tvar){
      return unify_var(tvar, b);
    },
    pattern(_, as<TypeVar>(arg)) = [&](auto& tvar){
      return unify_var(tvar, a);
    },
    pattern(_, _) = []() -> Substitution {
      throw UnifyError("can't unify types");
    }
  );
}

static void merge_left(Substitution& target, Substitution const & other) {
  // Merge 'other' into target (left)
  for (auto& [_, s]: target) {
    s = apply_type(s, other);
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

static Substitution unify(std::vector<Constraint>&& constraints) {
  if (constraints.size() == 0) {
    return {};
  }

  Constraint top_constraint = constraints.back();
  constraints.pop_back();
  auto subs = unify_one(top_constraint);
  apply_subs(constraints, subs);
  auto more_subs = unify(std::move(constraints));
  merge_left(subs, more_subs);
  return subs;
}

static Substitution unify(ConstraintSet&& constraints) {
  // Sets are not that fun to deal with & mutate
  return unify(std::vector<Constraint>(constraints.begin(), constraints.end()));
}

Substitution unify_and_apply(ConstraintSet && constraints) {
  SpecialConstraints special_constraints;
  std::vector<Constraint> constraints_vec;

  // Collect special constraints
  for (auto& [t1, t2]: constraints) {
    using namespace mpark::patterns;
    match(t1->v, t2->v)(
      pattern(as<TypeVar>(arg), as<TypeVar>(arg)) = [&](auto const& t1, auto const& t2) {
        WHEN(t2.special()) {
          special_constraints.push_back(std::make_pair(t1, t2));
        };
      },
      pattern(_,_) = [&]{
        constraints_vec.push_back(Constraint(t1, t2));
      }
    );
  }

  auto subs = unify(std::move(constraints_vec));
  if (!satisfies_special_constraints(subs, special_constraints)) {
    throw UnifyError("additional type constraints not met");
  }

  for (auto& [tvar, sub]: subs) {
    if (auto target = tvar.substitute.lock()) {
      if (occurs(TypeVar(TypeVar::ANY), sub)) {
        throw UnifyError("incomplete substitution");
      }
      *target = sub ? *sub : Type(VoidType{});
    }
  }
  return subs;
}

void generate_call_constraints(
  Type_Ptr& callee_type, Ast_Call const & call, ConstraintSet& constraints
) {
  if (std::holds_alternative<TypeVar>(callee_type->v)) {
    LambdaType call_type;
    call_type.return_type = to_type_ptr(TypeVar());
    std::generate_n(std::back_inserter(call_type.argument_types),
      call.arguments.size(), []{ return to_type_ptr(TypeVar()); });
    auto call_type_ptr = to_type_ptr(call_type);
    constraints.insert(Constraint(callee_type, call_type_ptr));
    callee_type = call_type_ptr;
  }
}

}
