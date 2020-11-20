#include <iostream>
#include <mpark/patterns.hpp>
#include <boost/range/combine.hpp>

#include "ast/visitor.h"
#include "ast/ast_builder.h"
#include "sema/type_infer.h"

namespace Infer {

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
        return current_tvar.id == tvar.id;
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
      ConstraintSet new_constraints;
      new_constraints.insert(Constraint(l1.return_type, l2.return_type));
      assert(l1.argument_types.size() == l2.argument_types.size());
      for (auto type_pair: boost::combine(l1.argument_types, l2.argument_types)) {
        Constraint arg_constraint;
        boost::tie(arg_constraint.first, arg_constraint.second) = type_pair;
        new_constraints.insert(arg_constraint);
      }
      return unify(std::move(new_constraints));
    },
    pattern(as<TypeVar>(_), as<TypeVar>(arg)) = [&](auto& tvar) {
      WHEN (tvar.special()) {
        /*
          Something like T1 = IntegerType
          They will always appear in this form due to how the constraints are generated.

          If this is unified first then we add:
            { IntegerType: T1 } to the solutions

            Then after all the solutions are solved if T1 is not an
            IntegerType, something has gone wrong -- fail, otherwise checks pass.

          If another constraint { T1 = Integer } is unified first then
            T1 = IntegerType may become { Integer = IntegerType }
            Which is also fine if T1 check fine then they all must be IntegerTypes.
        */
        return unify_var(tvar, a);
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

static bool satisfies_special_constraints(Substitution& subs) {
  auto sat_constraint = [&](TypeVar::Constraint constraint, Type const & type) {
    using namespace mpark::patterns;
    return match(type.v)(
      pattern(as<TypeVar>(_)) = []{ return true; },
      pattern(as<PrimativeType>(arg)) = [&](auto const & primative) {
        // The constraint can now be removed from the solutions.
        subs.erase(constraint);
        switch (constraint) {
          case TypeVar::NUMERIC: return primative.is_numeric_type();
          case TypeVar::INTEGER: return primative.is_integer_type();
          default: return false;
        }
      },
      pattern(_) = []{ return false; }
    );
  };

  for (auto const constraint: TypeVar::Constraints) {
    if (subs.contains(constraint)) {
      auto type = subs.at(constraint);
      if (!sat_constraint(constraint, *type)) {
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
  assert(satisfies_special_constraints(subs));
  return subs;
}

Substitution unify(ConstraintSet&& constraints) {
  // Sets are not that fun to deal with & mutate
  return unify(std::vector<Constraint>(constraints.begin(), constraints.end()));
}

Substitution unify_and_apply(ConstraintSet && constraints) {
  auto subs = unify(std::move(constraints));
  for (auto& [tvar, sub]: subs) {
    if (auto target = tvar.substitute.lock()) {
      *target = *sub;
    }
  }
  return subs;
}

}
