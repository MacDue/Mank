#include <mpark/patterns.hpp>
#include <boost/range/combine.hpp>

#include "ast/util.h"
#include "ast/visitor.h"
#include "ast/ast_builder.h"
#include "sema/types.h"
#include "sema/type_infer.h"

#include <iostream>

using SpecialConstraints = std::vector<std::pair<TypeVar, TypeVar>>;

/* Helpers */

static void extract_tvars(Type_Ptr type, std::set<TypeVar>& type_vars) {
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

void Infer::Constraint::init() {
  extract_tvars(t1, tvars_closure);
  extract_tvars(t2, tvars_closure);
}

Type_Ptr Infer::substitute(
  Type_Ptr current_type, TypeVar tvar, Type_Ptr replacement, Substitution const & subs
) {
  using namespace mpark::patterns;
  return match(current_type->v)(
    pattern(anyof(as<PrimativeType>(_), as<Ast_Pod_Declaration>(_))) = [&]{ return current_type; },
    pattern(as<LambdaType>(arg)) = [&](auto lambda_type) {
      lambda_type.return_type = substitute(
        lambda_type.return_type, tvar, replacement, subs);
      for (auto& arg_type: lambda_type.argument_types) {
        arg_type = substitute(arg_type, tvar, replacement, subs);
      }
      return ctx.new_type(lambda_type);
    },
    pattern(as<TupleType>(arg)) = [&](auto tuple_type) {
      for (auto& el_type: tuple_type.element_types) {
        el_type = substitute(el_type, tvar, replacement, subs);
      }
      return ctx.new_type(tuple_type);
    },
    pattern(as<TypeVar>(arg)) = [&](auto const & current_tvar) {
      // tvar already unified... to be replaced it needs to have already been unified
      if (current_tvar.id == tvar.id) {
        return replacement;
      }
      return current_type;
    },
    pattern(as<TypeFieldConstraint>(arg)) = [&](auto field_constraint) {
      field_constraint.type = substitute(field_constraint.type, tvar, replacement, subs);
      return ctx.new_type(field_constraint);
    },
    pattern(_) = [&]() -> Type_Ptr {
      throw UnifyError("unknown type substitution " + type_to_string(current_type.get()));
    }
  );
}

Type_Ptr Infer::apply_type(Type_Ptr type, Substitution const & subs) {
  auto result = type;
  for (auto& [tvar, solved_type]: subs) {
    result = substitute(result, tvar, solved_type, subs);
  }
  return result;
}

void Infer::apply_constraint(Constraint& c, Substitution const & subs) {
  c.t1 = apply_type(c.t1, subs);
  c.t2 = apply_type(c.t2, subs);
  c.init(); // update the constraints related tvars
}

void Infer::apply_subs(ConstraintSet& constraints, Substitution const & subs) {
  for (auto& c: constraints) {
    apply_constraint(c, subs);
  }
}

Infer::Substitution Infer::unify_var(TypeVar tvar, Type_Ptr type, ConstraintOrigin origin) {
  auto type_var = std::get_if<TypeVar>(&type->v);
  auto make_sub = [&]{
    if (origin) {
      unify_reasoning[tvar.id].push_back(std::make_pair(*origin, type));
    }
    return Substitution{{tvar, type}};
  };

  if (type_var) {
    if (type_var->id == tvar.id) {
      return Substitution{};
    } else {
      return make_sub();
    }
  } else if (occurs(tvar, type)) {
    throw UnifyError("circular type usage detected");
  } else {
    return make_sub();
  }
}

[[ noreturn ]]
void Infer::throw_unify_error(Constraint const & constraint) {
  std::cout << "bang?\n";
  this->top_failed_constraint = std::move(constraint);
  throw_compile_error(
    constraint.origin.value_or(SourceLocation{}),
    constraint.error_template,
    type_to_string(constraint.t1.get()),
    type_to_string(constraint.t2.get()));
}

Infer::Substitution Infer::try_unify_sub_constraints(
  Infer::Constraint const & parent,
  Infer::ConstraintSet&& sub_constraints
) {
  // Kinda a hack...
  try {
    auto [subs, e] = unify(std::move(sub_constraints));
    if (e) { throw_unify_error(parent); }
    return subs;
  } catch (...) {
    throw_unify_error(parent);
  }
}

Infer::Substitution Infer::unify_one(Infer::Constraint const & c) {
  using namespace mpark::patterns;

  std::cout << "unify: " << type_to_string(c.t1.get()) << " = " << type_to_string(c.t2.get()) << '\n';
  return match(c.t1->v, c.t2->v)(
    pattern(as<PrimativeType>(arg), as<PrimativeType>(arg)) = [](auto& p1, auto& p2){
      WHEN(p1.tag == p2.tag) {
        return Substitution{};
      };
    },
    pattern(as<LambdaType>(arg), as<LambdaType>(arg)) = [&](auto& l1, auto& l2) {
      WHEN(l1.argument_types.size() == l2.argument_types.size()) {
        ConstraintSet new_constraints;
        Constraint ret_constraint(l1.return_type, l2.return_type);
        new_constraints.emplace_back(ret_constraint);
        for (auto type_pair: boost::combine(l1.argument_types, l2.argument_types)) {
          Constraint arg_constraint;
          boost::tie(arg_constraint.t1, arg_constraint.t2) = type_pair;
          new_constraints.emplace_back(arg_constraint);
        }
        return try_unify_sub_constraints(c, std::move(new_constraints));
      };
    },
    pattern(as<TupleType>(arg), as<TupleType>(arg)) = [&](auto& t1, auto& t2) {
      WHEN(t1.element_types.size() == t2.element_types.size()) {
        ConstraintSet tuple_constraints;
        for (auto type_pair: boost::combine(t1.element_types, t2.element_types)) {
          Constraint tup_constraint;
          boost::tie(tup_constraint.t1, tup_constraint.t2) = type_pair;
          tuple_constraints.emplace_back(tup_constraint);
        }
        std::cout << "start:\n";
        auto ret = try_unify_sub_constraints(c, std::move(tuple_constraints));
        std::cout << "end\n";
        return ret;
      };
    },
    pattern(as<TypeFieldConstraint>(arg), _) = [&](auto field_constraint) {
      auto field_type = get_field_type(field_constraint.type, *field_constraint.field_access);
      Constraint fc{field_type, c.t2};
      return try_unify_sub_constraints(c, { fc });
    },
    pattern(_, as<TypeFieldConstraint>(arg)) = [&](auto field_constraint) {
      auto field_type = get_field_type(field_constraint.type, *field_constraint.field_access);
      Constraint fc{c.t1, field_type};
      return try_unify_sub_constraints(c, { fc });
    },
    pattern(as<TypeVar>(arg), _) = [&](auto& tvar){
      return unify_var(tvar, c.t2, c.origin);
    },
    pattern(_, as<TypeVar>(arg)) = [&](auto& tvar){
      return unify_var(tvar, c.t1, c.origin);
    },
    pattern(_, _) = [&]() -> Substitution { throw_unify_error(c); }
  );
}

void Infer::merge_left(Substitution& target, Substitution const & other) {
  // Merge 'other' into target (left)
  for (auto& [_, s]: target) {
    s = apply_type(s, other); // I think it's safe to ignore the origin here
  }
  target.insert(other.begin(), other.end());
}

static bool satisfies_special_constraints(
  Infer::Substitution const & subs,
  SpecialConstraints const & constraints
) {
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

Infer::UnifyResult Infer::unify(Infer::ConstraintSet&& constraints) {
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

// static std::vector<CompilerMessage> print_subs_reasons(int32_t tvar, Substitution const & subs) {
//   std::cout << tvar <<'\n';
//   std::vector<CompilerMessage> infer_info;
//   if (tvar >= 0 && subs_reasoning.contains(tvar)) {
//     // std::cout << "T" << tvar << " reasons:\n";
//     for (auto const & [loc, type]: subs_reasoning.at(tvar)) {
//       std::cout << loc.start_line << ':' << loc.start_column << " -> " << loc.end_line << ':' << loc.end_column << '\n';
//       if (!type) continue;
//       auto error_type = apply_type(type, subs, loc);
//       if (std::holds_alternative<TypeVar>(error_type->v)) {
//         continue; // not informative
//       }
//       infer_info.push_back(CompilerMessage{loc, "found to be " + type_to_string(error_type.get()), CompilerMessage::NOTE});
//     }
//     // std::cout << '\n';
//   }
//   return infer_info;
// }

Infer::Substitution Infer::unify_and_apply() {
  SpecialConstraints special_constraints;

  // Collect special constraints
  auto res = std::remove_if(type_constraints.begin(), type_constraints.end(), [&](auto& constraint) {
    using namespace mpark::patterns;
    constraint.init();
    return match(constraint.t1->v, constraint.t2->v)(
      pattern(as<TypeVar>(arg), as<TypeVar>(arg)) = [&](auto t1, auto t2) {
        WHEN(t2.special()) {
          special_constraints.push_back(std::make_pair(t1, t2));
          return true; // remove
        };
      },
      pattern(_,_) = [&]{
        std::cout << type_to_string(constraint.t1.get()) << " = "
          <<  type_to_string(constraint.t2.get())
          << " @" << constraint.origin->start_line
                  << ":" << constraint.origin->start_column << " -> "
                  << constraint.origin->end_line
                  << ":" << constraint.origin->end_column
                  << '\n';
        return false; }
    );
  });

  type_constraints.erase(res, type_constraints.end());

  // Since we want pop_back to return the first contraint so the errors make sense
  std::reverse(type_constraints.begin(), type_constraints.end());

  auto [subs, error] = unify(std::move(type_constraints));
  if (error) {
      std::vector<CompilerMessage> error_infer_info;

    // for (auto c: failed_constraint) {
    // std::cout << type_to_string(failed_constraint.types.first.get()) << " = " <<  type_to_string(failed_constraint.types.second.get()) << '\n';

    for (auto& tvar: top_failed_constraint.tvars_closure) {
      if (tvar.id < 0) continue;
      // auto infer_info = print_subs_reasons(tvar.id, subs);
      // error_infer_info.insert(error_infer_info.end(),
      //   std::make_move_iterator(infer_info.begin()),
      //   std::make_move_iterator(infer_info.end()));
      // print_subs_reasons(failed_constraint.tvar2, subs);
    }

    std::sort(error_infer_info.begin(), error_infer_info.end(),
      [](CompilerMessage const & m1, CompilerMessage const & m2){
        return std::make_pair(m1.location.start_line, m1.location.start_column)
          < std::make_pair(m2.location.start_line, m2.location.start_column);
      });


    // hack_backtrack_infer->insert(hack_backtrack_infer->end(),
    //   std::make_move_iterator(error_infer_info.begin()),
    //   std::make_move_iterator(error_infer_info.end()));

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
    if (occurs(TypeVar(TypeVar::ANY), sub)) {
      throw UnifyError("incomplete substitution");
    }
    assert(bool(sub) && "fix me! void not supported in infer");
    *(tvar.get_self().class_ptr()) = *sub;
  }
  // Ready for the next function.
  reset_inference();
  return subs;
}

void Infer::add_constraint(
  SourceLocation origin, Type_Ptr t1, Type_Ptr t2, char const * error_template
) {
  type_constraints.emplace_back(Constraint(origin, t1, t2, error_template));
}

void Infer::generate_call_constraints(Type_Ptr& callee_type, Ast_Call& call) {
  if (std::holds_alternative<TypeVar>(callee_type->v)) {
    LambdaType call_type;
    call_type.return_type = ctx.new_type(TypeVar());
    std::generate_n(std::back_inserter(call_type.argument_types),
      call.arguments.size(), [&]{ return ctx.new_type(TypeVar()); });
    auto call_type_ptr = ctx.new_type(call_type);
    Constraint constraint{
      AstHelper::extract_location(call.callee), callee_type, call_type_ptr,
      "call requires [{1}] but callee is [{0}]"};
    type_constraints.emplace_back(constraint);
    callee_type = call_type_ptr;
  }
}

void generate_array_index_constraints(
  Type_Ptr& array_type, Ast_Index_Access& index
) {
  // TODO
}

void Infer::generate_tuple_assign_constraints(Ast_Assign& tuple_assign) {
  auto tuple_type = tuple_assign.expression->meta.type;
  if (std::holds_alternative<TypeVar>(tuple_type->v)) {
    TupleType assign_type;
    std::generate_n(std::back_inserter(assign_type.element_types),
      std::get<Ast_Tuple_Literal>(tuple_assign.target->v).elements.size(),
      [&]{ return ctx.new_type(TypeVar()); });
    auto assign_type_ptr = ctx.new_type(assign_type);
    type_constraints.emplace_back(
      Constraint(AstHelper::extract_location(tuple_assign.expression),
        tuple_type, assign_type_ptr));
    tuple_assign.expression->meta.type = assign_type_ptr;
  }
}

std::optional<Infer::Constraint> Infer::generate_tuple_destructure_constraints(
  TupleBinding const & bindings, Type_Ptr& init_type, SourceLocation origin
) {
  // Almost the same as assign
  if (std::holds_alternative<TypeVar>(init_type->v)) {
    TupleType binding_type;
    std::generate_n(std::back_inserter(binding_type.element_types), bindings.binds.size(),
      [&]{ return ctx.new_type(TypeVar()); });
    auto binding_type_ptr = ctx.new_type(binding_type);
    Constraint constraint{origin, init_type, binding_type_ptr};
    constraint.error_template = "binding type {1} does not match init type {0}";
    init_type = binding_type_ptr;
    return constraint;
  }
  return std::nullopt;
}

