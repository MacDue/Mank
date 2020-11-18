#include <iostream>
#include <mpark/patterns.hpp>
#include <boost/range/combine.hpp>

#include "ast/visitor.h"
#include "ast/ast_builder.h"
#include "sema/type_infer.h"

namespace Infer {

static Type_Ptr substitute(
  Type_Ptr currentType, uint32_t type_id, Type_Ptr replacement,
  Substitution const & subs
) {
  using namespace mpark::patterns;
  return match(currentType->v)(
    pattern(as<PrimativeType>(_)) = [&]{ return currentType; },
    pattern(as<LambdaType>(arg)) = [&](auto lambda_type) {
      lambda_type.return_type = substitute(
        lambda_type.return_type, type_id, replacement, subs);
      for (auto& arg_type: lambda_type.argument_types) {
        arg_type = substitute(arg_type, type_id, replacement, subs);
      }
      return to_type_ptr(lambda_type);
    },
    pattern(as<TypeVar>(arg)) = [&](auto const & type_var) {
      if (!type_var.special_constraint && type_var.id == type_id) {
        return replacement;
      } // TODO special constraints
      return currentType;
    },
    pattern(_) = []() -> Type_Ptr {
      assert(false && "unknown type substitution");
      return nullptr;
    }
  );
}

static Type_Ptr apply_type(Type_Ptr type, Substitution const & subs) {
  auto result = type;
  for (auto& [type_id, solved_type]: subs) {
    result = substitute(result, type_id, solved_type, subs);
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

static bool occurs(uint32_t type_id, Type_Ptr type) {
  using namespace mpark::patterns;
  return match(type->v)(
    pattern(as<LambdaType>(arg)) =
      [&](auto const & lambda) {
        return occurs(type_id, lambda.return_type)
          || std::any_of(lambda.argument_types.begin(), lambda.argument_types.end(),
            [&](auto arg_type){ return occurs(type_id, arg_type); });
      },
    pattern(as<TypeVar>(arg)) =
      [&](auto const & type_var) {
        return type_id == type_var.id;
      },
    pattern(_) = []{ return false; });
}

static Substitution unify_var(TypeVar tvar, Type_Ptr type) {
  auto type_var = std::get_if<TypeVar>(&type->v);
  if (type_var) {
    if (type_var->id == tvar.id) {
      return Substitution{};
    } else {
      return Substitution{{tvar.id, type}};
    }
  } else if (occurs(tvar.id, type)) {
    assert(false && "circular type use");
  } else {
    return Substitution{{tvar.id, type}};
  }
}

static Substitution unify_one(Constraint constraint) {
  using namespace mpark::patterns;
  auto [a, b] = constraint;
  return match(a->v, b->v)(
    pattern(as<PrimativeType>(_), as<PrimativeType>(_)) = []{
      return Substitution{};
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
    pattern(as<TypeVar>(arg), _) = [&](auto& type_var){
      return unify_var(type_var, b);
    },
    pattern(_, as<TypeVar>(arg)) = [&](auto& type_var){
      return unify_var(type_var, a);
    },
    pattern(_, _) = []() -> Substitution {
      assert(false && "can't unify");
      return Substitution{};
    }
  );
}

// static void print_subs(Substitution const & subs) {
//   for (auto& [tvar, sub]: subs) {
//     std::cout << "T" << tvar << " -> " << type_to_string(sub.get()) << '\n';
//   }
// }

static void compose(Substitution& a, Substitution const & b) {
  for (auto& [_, s]: a) {
    s = apply_type(s, b);
  }
  a.insert(b.begin(), b.end());
}

static Substitution unify(std::vector<Constraint>&& constraints) {
  if (constraints.size() == 0) {
    return {};
  }

  Constraint top_constraint = constraints.back();
  constraints.pop_back();
  auto subs = unify_one(top_constraint);
  // std::cout << "Unify one:\n";
  // print_subs(subs);
  apply_subs(constraints, subs);
  // std::cout << "Constraints\n";
  // for (auto& [a, b]: constraints) {
  //   std::cout << type_to_string(a.get()) << " = " << type_to_string(b.get()) << '\n';
  // }

  auto more_subs = unify(std::move(constraints));
  compose(subs, more_subs);
  // std::cout << '\n';
  // std::cout << "Unify Rest:\n";
  // print_subs(subs);
  return subs;
}

Substitution unify(ConstraintSet&& constraints) {
  // Sets are not that fun to deal with & mutate
  return unify(std::vector<Constraint>(constraints.begin(), constraints.end()));
}

}
