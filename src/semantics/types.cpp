#include "sema/types.h"

static bool match_type_lists(
  std::vector<Type_Ptr> const & a,
  std::vector<Type_Ptr> const & b,
  Infer::ConstraintSet* constraints,
  Infer::Constraint blank_constraint,
  bool ignore_refs
) {
  if (a.size() != b.size()) {
    return false;
  }
  for (size_t idx = 0; idx < a.size(); idx++) {
    if (!match_types(a.at(idx), b.at(idx), constraints, blank_constraint, ignore_refs)) {
      return false;
    }
  }
  return true;
}

bool match_types(Type_Ptr a, Type_Ptr b,
  Infer::ConstraintSet* constraints,
  Infer::Constraint blank_constraint,
  bool ignore_refs
) {
  using namespace mpark::patterns;

  if (ignore_refs) {
    // Reference types should be matched like normal versions of their type
    // only when binding them should their be special treatment
    // Update: this only applies at the top level
    // \x: ref -> { x } != \x -> { x }
    a = remove_reference(a);
    b = remove_reference(b);
  }

  if (constraints) {
    auto min_t = min_type(a, b);
    if (min_t && std::holds_alternative<TypeVar>(min_t->v)) {
      auto new_constraint = blank_constraint;
      new_constraint.types = std::make_pair(a, b);
      constraints->push_back(new_constraint);
      return true;
    }
  }

  if (a == b) {
    return true;
  } else if (a && b) {
    return match(a->v, b->v)(
      pattern(as<PrimativeType>(arg), as<PrimativeType>(arg)) =
        [](auto const & a, auto const & b) {
          return a.tag == b.tag;
        },
      pattern(as<FixedSizeArrayType>(arg), as<FixedSizeArrayType>(arg)) =
        [&](auto const & a, auto const & b) {
           // refs can't appear in arrays
          return a.size == b.size
            && match_types(a.element_type, b.element_type, constraints, blank_constraint);
        },
      pattern(as<LambdaType>(arg), as<LambdaType>(arg)) =
        [&](auto const & a, auto const & b) {
          return match_type_lists(a.argument_types, b.argument_types, constraints, blank_constraint, false)
            && match_types(a.return_type, b.return_type, constraints, blank_constraint, false);
        },
      pattern(as<TupleType>(arg), as<TupleType>(arg)) =
        [&](auto const & a, auto const & b) {
          // Don't match (ref i32, i32) = (i32, i32)
          // (will need special case for patterns)
          return match_type_lists(a.element_types, b.element_types, constraints, blank_constraint, false);
        },
      pattern(as<TypeVar>(arg), as<TypeVar>(arg)) =
        [](auto const & a, auto const & b) {
          return a.id == b.id;
        },
      pattern(_, _) = []{ return false; });
  }
  return false;
}

TypeResolution resolve_type(Scope& scope, Type_Ptr type) {
  using namespace mpark::patterns;
  static auto null_symbol = std::optional<Ast_Identifier>{};

  return match(type->v)(
    pattern(as<UncheckedType>(arg)) = [&](auto const & unchecked) {
      Symbol* symbol = scope.lookup_first_name(unchecked.identifier);
      auto resolved_type = symbol && symbol->kind == Symbol::TYPE
        ? symbol->type : nullptr;
      return std::make_pair(resolved_type, std::optional{unchecked.identifier});
    },
    pattern(as<FixedSizeArrayType>(arg)) = [&](auto& array_type) {
      auto [element_type, symbol] = resolve_type(scope, array_type.element_type);
      array_type.element_type = element_type;
      return std::make_pair(element_type ? type : nullptr, symbol);
    },
    pattern(as<ReferenceType>(arg)) = [&](auto& reference_type) {
      auto [referenced_type, symbol] = resolve_type(scope, reference_type.references);
      reference_type.references = referenced_type;
      return std::make_pair(referenced_type ? type : nullptr, symbol);
    },
    pattern(as<LambdaType>(arg)) = [&](auto& lambda_type) {
      if (lambda_type.return_type) { // hack solution to returing nothing
        auto [return_type, return_symbol] = resolve_type(scope, lambda_type.return_type);
        lambda_type.return_type = return_type;

        if (!return_type) {
          return std::make_pair(Type_Ptr(nullptr), return_symbol);
        }
      }

      for (auto& arg_type: lambda_type.argument_types) {
        auto [resolved_arg_type, arg_symbol] = resolve_type(scope, arg_type);
        arg_type = resolved_arg_type;
        if (!arg_type) {
          return std::make_pair(Type_Ptr(nullptr), arg_symbol);
        }
      }

      return std::make_pair(type, null_symbol);
    },
    pattern(as<TupleType>(arg)) = [&](auto& tuple_type) {
      for (auto& el_type: tuple_type.element_types) {
        auto [resolved_el_type, el_symbol] = resolve_type(scope, el_type);
        el_type = resolved_el_type;
        if (!el_type) {
          return std::make_pair(Type_Ptr(nullptr), el_symbol);
        }
      }
      return std::make_pair(type, null_symbol);
    },
    pattern(_) = [&] {
      // Possible it's already resolved
      // TODO: Make sure code does not recurse over already resolved types.
      return std::make_pair(type, null_symbol);
    });
}

static std::pair<Ast_Argument*, int> resolve_pod_field_index(
  Ast_Pod_Declaration& pod_type, std::string_view field_name
) {
  auto accessed = std::find_if(pod_type.fields.begin(), pod_type.fields.end(),
    [&](auto& field) {
      return field.name.name == field_name;
    });
  if (accessed == pod_type.fields.end()) {
    return std::make_pair(nullptr, -1);
  }
  auto index = std::distance(pod_type.fields.begin(), accessed);
  return std::make_pair(&(*accessed), index);
}

Type_Ptr get_field_type(Type* type, Ast_Field_Access& access) {
  using namespace mpark::patterns;
  if (type) {
    auto access_type = match(remove_reference(type)->v)(
      pattern(as<Ast_Pod_Declaration>(arg)) = [&](auto& pod_type) {
        auto [accessed, field_index] = resolve_pod_field_index(pod_type, access.field.name);
        if (field_index < 0) {
          throw_sema_error_at(access.field, "{} has no field named \"{}\"",
              pod_type.identifier.name, access.field.name);
        }
        access.field_index = field_index;
        access.get_meta().value_type = access.object->meta.value_type;
        // expr.inherit_value_type(*access.object);
        return accessed->type;
      },
      // FIXME: Hardcoded .length
      pattern(as<FixedSizeArrayType>(_)) = [&]{
        WHEN(access.field.name == "length") {
          return Primative::INTEGER;
        };
      },
      pattern(_) = []() -> Type_Ptr { return nullptr; });
    if (access_type) {
      return access_type;
    }
  }
  throw_sema_error_at(access.object, "not a pod type");
}
