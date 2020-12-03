#include <mpark/patterns.hpp>
#include <boost/range/combine.hpp>

#include "ast/util.h"
#include "ast/visitor.h"
#include "ast/ast_builder.h"
#include "sema/types.h"
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
    pattern(anyof(as<PrimativeType>(_), as<Ast_Pod_Declaration>(_))) = [&]{ return current_type; },
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
      // tvar already unified... to be replaced it needs to have already been unified
      if (current_tvar.id == tvar.id) {
        // if (auto rtvar = std::get_if<TypeVar>(&replacement->v)) {
        //   // rtvar already unified
        //   if (!subs_reasoning.contains(current_tvar.id)) {
        //     // since current_tvar.id may not get unified now
        //     subs_reasoning[current_tvar.id] = subs_reasoning[rtvar->id];
        //   }
        // }
        //   std::cout << "tvar replaced " << current_tvar.id << " " << rtvar->id << '\n';
        //   auto& reasons = subs_reasoning[rtvar->id];
        //   auto& pior_reasons = subs_reasoning[current_tvar.id];
        //   reasons.insert(reasons.end(), pior_reasons.begin(), pior_reasons.end());
        //   pior_reasons = reasons;
        // } // } else {
        //    subs_reasoning[current_tvar.id].push_back(std::make_pair(origin, replacement));
        // }

        return replacement;
      }
      return current_type;
    },
    pattern(as<TypeFieldConstraint>(arg)) = [&](auto field_constraint) {
      field_constraint.type = substitute(field_constraint.type, tvar, replacement, subs, origin);
      return to_type_ptr(field_constraint);
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
  c.init();
}

static void apply_subs(std::vector<Constraint>& constraints, Substitution const & subs
) {
  for (auto& c: constraints) {
    apply_constraint(c, subs);
  }
}

void extract_tvars(Type_Ptr type, std::set<TypeVar>& type_vars) {
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
    pattern(as<TypeVar>(arg)) = [&](auto tvar){ type_vars.insert(tvar); },
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
        subs_reasoning[tvar.id].push_back(std::make_pair(origin, type));
      }
      return Substitution{{tvar, type}};
    }
  } else if (occurs(tvar, type)) {
    throw UnifyError("circular type usage detected");
  } else {
    if (!do_not_add_reasons) {
      // std::cout << "unify: " << tvar.id << " " << type_to_string(type.get()) << '\n';
      subs_reasoning[tvar.id].push_back(std::make_pair(origin, type));
    }
    return Substitution{{tvar, type}};
  }
}

[[ noreturn ]]
static void throw_unify_error(Constraint const & constraint) {
      std::cout << "bang?\n";
  // throw false;
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
    auto [subs, e] = unify(std::move(sub_constraints));
    if (e) { do_not_add_reasons = false; throw_unify_error(parent); }
    return subs;
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
    std::cout << "here????\n";
    throw_unify_error(parent);
  }
  do_not_add_reasons = false;
}

static Substitution unify_one(Constraint constraint) {

  using namespace mpark::patterns;
  auto [a, b] = constraint.types;
            std::cout << "unify: " << type_to_string(a.get()) << " = " << type_to_string(b.get()) << '\n';

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
        std::cout << "start:\n";
        auto ret = try_unify_sub_constraints(constraint, std::move(tuple_constraints));
        std::cout << "end\n";
        return ret;
      };
    },
    pattern(as<TypeFieldConstraint>(arg), _) = [&](auto field_constraint) {
      auto field_type = get_field_type(field_constraint.type.get(), *field_constraint.field_access);
      Constraint fc = constraint;
      fc.types.first = field_type;
      fc.types.second = b;
      return try_unify_sub_constraints(constraint, { fc });
    },
    pattern(_, as<TypeFieldConstraint>(arg)) = [&](auto field_constraint) {
      auto field_type = get_field_type(field_constraint.type.get(), *field_constraint.field_access);
      Constraint fc = constraint;
      fc.types.first = a;
      fc.types.second = field_type;
      return try_unify_sub_constraints(constraint, { fc });
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
    std::cout << "error!\n";
    return std::make_pair(subs, e);
  }
}

static std::vector<CompilerMessage> print_subs_reasons(int32_t tvar, Substitution const & subs) {
  std::cout << tvar <<'\n';
  std::vector<CompilerMessage> infer_info;
  if (tvar >= 0 && subs_reasoning.contains(tvar)) {
    // std::cout << "T" << tvar << " reasons:\n";
    for (auto const & [loc, type]: subs_reasoning.at(tvar)) {
      std::cout << loc.start_line << ':' << loc.start_column << " -> " << loc.end_line << ':' << loc.end_column << '\n';
      if (!type) continue;
      auto error_type = apply_type(type, subs, loc);
      if (std::holds_alternative<TypeVar>(error_type->v)) {
        continue; // not informative
      }
      infer_info.push_back(CompilerMessage{loc, "found to be " + type_to_string(error_type.get()), CompilerMessage::NOTE});
    }
    // std::cout << '\n';
  }
  return infer_info;
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
      pattern(_,_) = [&]{
        std::cout << type_to_string(constraint.types.first.get()) << " = "
          <<  type_to_string(constraint.types.second.get())
          << " @" << constraint.origin.start_line
                  << ":" << constraint.origin.start_column << " -> "
                  << constraint.origin.end_line
                  << ":" << constraint.origin.end_column
                  << '\n';
        return false; }
    );
  });

  constraints.erase(res, constraints.end());

  // Since we want pop_back to return the first contraint so the errors make sense
  std::reverse(constraints.begin(), constraints.end());


  auto [subs, error] = unify(std::move(constraints));

  if (error) {
      std::vector<CompilerMessage> error_infer_info;

    // for (auto c: failed_constraint) {
        std::cout << type_to_string(failed_constraint.types.first.get()) << " = " <<  type_to_string(failed_constraint.types.second.get()) << '\n';

    for (auto& tvar: failed_constraint.contained_tvars) {
      if (tvar.id < 0) continue;
      auto infer_info = print_subs_reasons(tvar.id, subs);
      error_infer_info.insert(error_infer_info.end(),
        std::make_move_iterator(infer_info.begin()),
        std::make_move_iterator(infer_info.end()));
      // print_subs_reasons(failed_constraint.tvar2, subs);
    }
    subs_reasoning = {}; // proto hack

    std::sort(error_infer_info.begin(), error_infer_info.end(),
      [](CompilerMessage const & m1, CompilerMessage const & m2){
        return std::make_pair(m1.location.start_line, m1.location.start_column)
          < std::make_pair(m2.location.start_line, m2.location.start_column);
      });


    hack_backtrack_infer->insert(hack_backtrack_infer->end(),
      std::make_move_iterator(error_infer_info.begin()),
      std::make_move_iterator(error_infer_info.end()));

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

std::optional<Constraint> generate_tuple_destructure_constraints(
  TupleBinding const & bindings, Type_Ptr& init_type, SourceLocation loc
) {
  // Almost the same as assign
  if (std::holds_alternative<TypeVar>(init_type->v)) {
    TupleType binding_type;
    std::generate_n(std::back_inserter(binding_type.element_types), bindings.binds.size(),
      []{ return to_type_ptr(TypeVar()); });
    auto binding_type_ptr = to_type_ptr(binding_type);
    // FIXME: Add loc
    auto constraint = Constraint(loc, init_type, binding_type_ptr);
    constraint.error_template = "binding type {1} does not match init type {0}";
    // constraints.push_back(constraint);
    init_type = binding_type_ptr;
    return constraint;
  }
  return std::nullopt;
}

}
