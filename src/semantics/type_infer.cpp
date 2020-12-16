#include <mpark/patterns.hpp>
#include <boost/range/combine.hpp>

#include "ast/util.h"
#include "ast/visitor.h"
#include "ast/ast_builder.h"
#include "sema/types.h"
#include "sema/type_infer.h"

#include <iostream>

struct SpecialConstraint {
  TypeVar t, constraint;
  SourceLocation origin;
};

using SpecialConstraints = std::vector<SpecialConstraint>;

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
    // if (type_var->id == tvar.id) {
    //   return Substitution{};
    // } else {
      return make_sub(); // TODO: make not add reason if tvars the same?
    // }
  } else if (occurs(tvar, type)) {
    throw UnifyError("circular type usage detected");
  } else {
    return make_sub();
  }
}

[[ noreturn ]]
void Infer::throw_unify_error(Constraint const & constraint) {
  // std::cout << "bang?\n";
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

  // std::cout << "unify: " << type_to_string(c.t1.get()) << " = " << type_to_string(c.t2.get()) << '\n';
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
        // std::cout << "start:\n";
        auto ret = try_unify_sub_constraints(c, std::move(tuple_constraints));
        // std::cout << "end\n";
        return ret;
      };
    },
    pattern(as<Ast_Pod_Declaration>(arg), as<Ast_Pod_Declaration>(arg)) = [&](auto& p1, auto& p2) {
      WHEN(p1.identifier.name == p2.identifier.name) {
        return Substitution{};
      };
    },
    pattern(as<TypeFieldConstraint>(arg), _) = [&](auto field_constraint) {
      auto field_type = get_field_type(
        field_constraint.type, *field_constraint.field_access, resolved_pods);
      Constraint fc{field_type, c.t2};
      return try_unify_sub_constraints(c, { fc });
    },
    pattern(_, as<TypeFieldConstraint>(arg)) = [&](auto field_constraint) {
      auto field_type = get_field_type(
        field_constraint.type, *field_constraint.field_access, resolved_pods);
      Constraint fc{c.t1, field_type};
      return try_unify_sub_constraints(c, { fc });
    },
    pattern(as<TypeVar>(arg), _) = [&](auto& tvar){
      return unify_var(tvar, c.t2, c.get_note_spot());
    },
    pattern(_, as<TypeVar>(arg)) = [&](auto& tvar){
      return unify_var(tvar, c.t1, c.get_note_spot());
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
  SpecialConstraints const & constraints,
  SpecialConstraint const ** failed_constraint
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

  for (auto& sc: constraints) {
    if (subs.contains(sc.t)) {
      auto type = subs.at(sc.t);
      if (!sat_constraint(static_cast<TypeVar::Constraint>(sc.constraint.id), *type)) {
        *failed_constraint = &sc;
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
    // std::cout << "error!\n";
    return std::make_pair(subs, e);
  }
}

void Infer::get_infer_reason_notes(
  int32_t tvar, std::vector<CompilerMessage>& msgs, Infer::Substitution const & subs
) {
  std::vector<CompilerMessage> infer_info;
  if (tvar >= 0 && unify_reasoning.contains(tvar)) {
    for (auto const & [loc, type]: unify_reasoning.at(tvar)) {
      // std::cout << loc.start_line << ':' << loc.start_column << " -> " << loc.end_line << ':' << loc.end_column << '\n';
      if (!type) continue;
      auto error_type = apply_type(type, subs);
      if (is_tvar(error_type)) {
        continue; // not informative
      }
      msgs.push_back(CompilerMessage{loc, "found to be " + type_to_string(error_type.get()), CompilerMessage::NOTE});
    }
  }
}

Infer::Substitution Infer::unify_and_apply() {
  SpecialConstraints special_constraints;

  // Collect special constraints
  auto res = std::remove_if(type_constraints.begin(), type_constraints.end(), [&](auto& constraint) {
    using namespace mpark::patterns;
    constraint.init();
    return match(constraint.t1->v, constraint.t2->v)(
      pattern(as<TypeVar>(arg), as<TypeVar>(arg)) = [&](auto t1, auto t2) {
        WHEN(t2.special()) {
          special_constraints.push_back(
            SpecialConstraint{.t = t1, .constraint = t2, .origin = *constraint.origin});
          return true; // remove
        };
      },
      pattern(_,_) = [&]{
        // std::cout << type_to_string(constraint.t1.get()) << " = "
        //   <<  type_to_string(constraint.t2.get())
        //   << " @" << constraint.origin->start_line
        //           << ":" << constraint.origin->start_column << " -> "
        //           << constraint.origin->end_line
        //           << ":" << constraint.origin->end_column
        //           << '\n';
        return false;
      }
    );
  });

  type_constraints.erase(res, type_constraints.end());

  // Since we want pop_back to return the first contraint so the errors make sense
  std::reverse(type_constraints.begin(), type_constraints.end());

  auto [subs, error] = unify(std::move(type_constraints));
  if (error) {
    /// Handle and display unify errors
    std::vector<CompilerMessage> infer_reason_notes;

    for (auto& tvar: top_failed_constraint.tvars_closure) {
      if (tvar.id < 0) continue;
      get_infer_reason_notes(tvar.id, infer_reason_notes, subs);
    }

    // Sort by pos in file (so it looks like there's some flow to it)
    std::sort(infer_reason_notes.begin(), infer_reason_notes.end(),
      [](CompilerMessage const & m1, CompilerMessage const & m2){
        return std::make_pair(m1.location.start_line, m1.location.start_column)
          < std::make_pair(m2.location.start_line, m2.location.start_column);
      });

    // FIXME: Hack to ensure no duplicate notes added
    auto last_note = std::unique(
      infer_reason_notes.begin(), infer_reason_notes.end(),
      [&](auto& a, auto& b){
        return a.location == b.location && a.message == b.message;
      });
    infer_reason_notes.erase(last_note, infer_reason_notes.end());
    std::for_each(infer_reason_notes.begin(), infer_reason_notes.end(), add_message);

    throw *error;
  }

  SpecialConstraint const * failed_special_constraint = nullptr;
  if (!satisfies_special_constraints(subs, special_constraints, &failed_special_constraint)) {
    // Special constraint errors
    // TODO: Get notes working here too?
    throw_compile_error(failed_special_constraint->origin,
      "was expecting a {} type but inferred {}",
      type_to_string((&failed_special_constraint->constraint).class_ptr().get()),
      type_to_string(subs.at(failed_special_constraint->t).get()));
  }

  for (auto& [tvar, sub]: subs) {
    if (occurs(TypeVar(TypeVar::ANY), sub)) {
      if (unify_reasoning.contains(tvar.id)) {
        auto [loc, _] = unify_reasoning.at(tvar.id).at(0);
        throw_compile_error(loc, "type unknown! maybe add a type annotation?");
      }
      throw UnifyError("incomplete substitution");
    }
    assert(bool(sub) && "fix me! missing type in infer sub");
    *(tvar.get_self().class_ptr()) = *sub;
  }

  // Ready for the next function.
  reset_inference();
  return subs;
}

void Infer::add_constraint(
  SourceLocation origin, Type_Ptr t1, Type_Ptr t2, char const * error_template,
  Infer::ConstraintOrigin note_spot
) {
  type_constraints.emplace_back(Constraint(origin, t1, t2, error_template, note_spot));
}

bool Infer::match_or_constrain_types_at(
  SourceLocation loc, Type_Ptr t1, Type_Ptr t2, char const* error_template,
  Infer::ConstraintOrigin note_spot
) {
  if (!match_types(t1, t2, or_constrain(loc, error_template, note_spot))) {
    throw_compile_error(loc, error_template,
      type_to_string(t1.get()), type_to_string(t2.get()));
  }
  return true;
}

void Infer::generate_call_constraints(Type_Ptr& callee_type, Ast_Call& call) {
  if (is_tvar(callee_type)) {
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
  if (is_tvar(tuple_type)) {
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
  if (is_tvar(init_type)) {
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

