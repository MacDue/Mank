#include "sema/types.h"

static bool match_type_lists(
  std::vector<Type_Ptr> const & a,
  std::vector<Type_Ptr> const & b,
  MakeConstraint const & make_constraint,
  bool ignore_refs
) {
  if (a.size() != b.size()) {
    return false;
  }
  for (size_t idx = 0; idx < a.size(); idx++) {
    if (!match_types(a.at(idx), b.at(idx), make_constraint, ignore_refs)) {
      return false;
    }
  }
  return true;
}

bool match_types(Type_Ptr a, Type_Ptr b,
  MakeConstraint const & make_constraint,
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

  if (make_constraint) {
    auto min_t = min_type(a, b);
    if (is_tvar(min_t)) {
      (*make_constraint)(a, b);
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
      pattern(as<PodType>(arg), as<PodType>(arg)) =
        [](auto const & a, auto const & b) {
          return a.identifier.name == b.identifier.name;
        },
      pattern(as<EnumType>(arg), as<EnumType>(arg)) =
        [](auto const & a, auto const & b) {
          return a.identifier.name == b.identifier.name;
        },
      pattern(as<FixedSizeArrayType>(arg), as<FixedSizeArrayType>(arg)) =
        [&](auto const & a, auto const & b) {
           // refs can't appear in arrays
          return a.size == b.size
            && match_types(a.element_type, b.element_type, make_constraint);
        },
      pattern(as<ListType>(arg), as<ListType>(arg)) =
        [&](auto const & a, auto const & b) {
          return match_types(a.element_type, b.element_type, make_constraint);
        },
      pattern(as<LambdaType>(arg), as<LambdaType>(arg)) =
        [&](auto const & a, auto const & b) {
          return match_type_lists(a.argument_types, b.argument_types, make_constraint, false)
            && match_types(a.return_type, b.return_type, make_constraint, false);
        },
      pattern(as<TupleType>(arg), as<TupleType>(arg)) =
        [&](auto const & a, auto const & b) {
          // Don't match (ref i32, i32) = (i32, i32)
          // (will need special case for patterns)
          return match_type_lists(a.element_types, b.element_types, make_constraint, false);
        },
      pattern(as<TypeVar>(arg), as<TypeVar>(arg)) =
        [](auto const & a, auto const & b) {
          return a.id == b.id;
        },
      pattern(_, _) = []{ return false; });
  }
  return false;
}

TypeResolution resolve_type(Scope& scope, Type_Ptr type, TypeStarts starting_points) {
  using namespace mpark::patterns;
  static auto null_symbol = std::optional<Ast_Identifier>{};

  return match(type->v)(
    pattern(as<UncheckedType>(arg)) = [&](auto& unchecked) {
      Symbol* symbol = scope.lookup_first_name(unchecked.identifier);
      auto resolved_type = symbol && symbol->kind == Symbol::TYPE
        ? symbol->type : nullptr;
      if (starting_points) {
        if (starting_points->contains(unchecked.identifier)) {
          throw_error_at(unchecked.identifier, "infinite type detected");
        }
        if (auto nested_type = std::get_if<UncheckedType>(&resolved_type->v)) {
          starting_points->insert(unchecked.identifier);
          /*
            If think we only need to resolve futher here for the case of
            type A = i32;
            type C = A[];
            where A resolved directly to a unchecked type.
            If a were a type like \i32 -> i32 it's members would be
            resolved later.
          */
          return resolve_type(scope, resolved_type, starting_points);
        }
      }
      return std::make_pair(resolved_type, std::optional{unchecked.identifier});
    },
    pattern(as<FixedSizeArrayType>(arg)) = [&](auto& array_type) {
      auto [element_type, symbol] = resolve_type(
        scope, array_type.element_type, starting_points);
      array_type.element_type = element_type;
      return std::make_pair(element_type ? type : nullptr, symbol);
    },
    pattern(as<ReferenceType>(arg)) = [&](auto& reference_type) {
      auto [referenced_type, symbol] = resolve_type(
        scope, reference_type.references, starting_points);
      reference_type.references = referenced_type;
      return std::make_pair(referenced_type ? type : nullptr, symbol);
    },
    pattern(as<LambdaType>(arg)) = [&](auto& lambda_type) {
      if (lambda_type.return_type) { // hack solution to returing nothing
        auto [return_type, return_symbol] = resolve_type(
          scope, lambda_type.return_type, starting_points);
        lambda_type.return_type = return_type;

        if (!return_type) {
          return std::make_pair(Type_Ptr(nullptr), return_symbol);
        }
      }

      for (auto& arg_type: lambda_type.argument_types) {
        auto [resolved_arg_type, arg_symbol] = resolve_type(
          scope, arg_type, starting_points);
        arg_type = resolved_arg_type;
        if (!arg_type) {
          return std::make_pair(Type_Ptr(nullptr), arg_symbol);
        }
      }

      return std::make_pair(type, null_symbol);
    },
    pattern(as<TupleType>(arg)) = [&](auto& tuple_type) {
      for (auto& el_type: tuple_type.element_types) {
        auto [resolved_el_type, el_symbol] = resolve_type(
          scope, el_type, starting_points);
        el_type = resolved_el_type;
        if (!el_type) {
          return std::make_pair(Type_Ptr(nullptr), el_symbol);
        }
      }
      return std::make_pair(type, null_symbol);
    },
    pattern(as<ListType>(arg)) = [&](auto& list_type) {
      auto [base_type, symbol] = resolve_type(
        scope, list_type.element_type, starting_points);
      list_type.element_type = base_type;
      return std::make_pair(base_type ? type : nullptr, symbol);
    },
    pattern(_) = [&] {
      // Possible it's already resolved
      // TODO: Make sure code does not recurse over already resolved types.
      return std::make_pair(type, null_symbol);
    });
}

#define TYPE_MUST_BE_KNOWN "type must be known here! Maybe add a type annotation?"

static std::pair<Type_Ptr,int> get_field_type(
  Type_Ptr type,
  Expr_Ptr object,
  Ast_Identifier const & field,
  Expression_Meta::ValueType& value_type
) {
  using namespace mpark::patterns;
  if (type) {
    int resolved_field_index = -1;
    auto access_type = match(remove_reference(type)->v)(
      pattern(as<PodType>(arg)) = [&](auto& pod_type) {
        auto field_info = pod_type.get_field(field);
        // Only update value type here (all others are always rvalues)
        value_type = object->meta.value_type;
        resolved_field_index = field_info.index;
        return field_info.type;
      },
      // FIXME: Hardcoded .length
      pattern(anyof(as<FixedSizeArrayType>(_), as<ListType>(_))) = [&]{
        WHEN(field.name == "length") {
          return PrimativeType::int_ty();
        };
      },
      pattern(as<PrimativeType>(arg)) = [&](auto& primative_type) {
        WHEN(primative_type.is_string_type() && field.name == "length") {
          return PrimativeType::int_ty();
        };
      },
      pattern(as<CellType>(arg)) = [&](auto& cell_type){
        WHEN(field.name == "value") {
          value_type = Expression_Meta::LVALUE;
          return cell_type.ref;
        };
      },
      pattern(as<EnumType>(_)) = [&]{
        WHEN(field.name == "tag") {
          return PrimativeType::int_ty();
        };
      },
      pattern(as<TypeVar>(_)) = [&]() -> Type_Ptr {
        throw_error_at(object, TYPE_MUST_BE_KNOWN);
        return nullptr;
      },
      pattern(_) = []() -> Type_Ptr { return nullptr; });
    if (access_type) {
      return std::make_pair(access_type, resolved_field_index);
    }
  }
  throw_error_at(object, "not a pod type (is {})", type_to_string(type.get()));
}

Type_Ptr get_field_type(Ast_Field_Access& access) {
  auto [access_type, field_index] = get_field_type(
    access.object->meta.type,
    access.object,
    access.field,
    access.get_meta().value_type);
  access.field_index = field_index;
  return access_type;
}

Type_Ptr get_field_type(TypeFieldConstraint& field_constraint) {
  Expression_Meta::ValueType value_type;
  if (auto pior_value_type = field_constraint.get_value_type()) {
    value_type = *pior_value_type;
  }
  auto [access_type, field_index] = get_field_type(
    field_constraint.type,
    field_constraint.get_object(),
    field_constraint.get_field(),
    value_type);
  field_constraint.resolve_field_index(field_index);
  // If there was no pior value is this a NOP.
  field_constraint.resolve_value_type(value_type);
  return access_type;
}

Type_Ptr get_field_type(
  Ast_Pod_Bind& pod_bind, Expr_Ptr init, Type_Ptr init_type
) {
  Expression_Meta::ValueType _value_type; // no needed
  auto [access_type, field_index] = get_field_type(
    init_type,
    init,
    pod_bind.field,
    _value_type);
  pod_bind.field_index = field_index;
  return access_type;
}

Type_Ptr get_element_type(Type_Ptr type, Ast_Index_Access& access) {
  using namespace mpark::patterns;
  return match(remove_reference(type)->v)(
    pattern(as<FixedSizeArrayType>(arg)) = [&](auto& array_type) {
      access.get_meta().value_type = access.object->meta.value_type;
      access.object->meta.type = type; // hack for bounds check
      static_check_array_bounds(access);
      return array_type.element_type;
    },
    pattern(as<PrimativeType>(arg)) = [](auto& primative_type) {
      WHEN(primative_type.is_string_type()) {
        // lvalue string indexs...?
        return PrimativeType::char_ty();
      };
    },
    pattern(as<ListType>(arg)) = [&](auto& list_type) {
      access.get_meta().value_type = access.object->meta.value_type;
      return list_type.element_type;
    },
    pattern(_) = [&]() -> Type_Ptr {
      throw_error_at(access.object, "not an indexable type (is {})",
        type_to_string(type.get()));
      return nullptr;
    }
  );
}

void static_check_array_bounds(Ast_Index_Access& index_access, bool allow_missing_types) {
  if (auto index_value = index_access.index->meta.get_const_value()) {
    auto object_type = index_access.object->meta.type;
    // FIXME: Will break if more indexable types added
    auto array_type = get_if_dereferenced_type<FixedSizeArrayType>(object_type);
    if (!array_type) {
      // Would only be a tvar
      if (allow_missing_types) return;
      throw_error_at(index_access.object, TYPE_MUST_BE_KNOWN);
    }
    // assert(array_type && "should be an array type");
    // FIXME: Will break with more integer types
    auto int_index = std::get<int32_t>(*index_value);
    if (int_index < 0 || static_cast<size_t>(int_index) >= array_type->size) {
      throw_error_at(index_access.index, "out of bounds index");
    }
  }
}

static auto cast_closure(
  std::set<std::pair<PrimativeType::Tag, PrimativeType::Tag>> casts
){
  while (true) {
    auto new_casts = casts;
    for (auto [t1, t2]: casts) { for (auto [t3, t4]: casts) {
      if (t2 == t3) {
        new_casts.insert(std::make_pair(t1, t4));
      }
    }}
    if (new_casts.size() == casts.size()) {
      break;
    }
    casts = new_casts;
  }
  return casts;
};

bool validate_type_cast(Type_Ptr source_type, Ast_As_Cast& as_cast) {
  using namespace mpark::patterns;
  if (match_types(source_type, as_cast.type, std::nullopt, false)) {
    return true;
  }
  bool valid_cast = match(remove_reference(source_type)->v, as_cast.type->v)(
    pattern(as<PrimativeType>(arg), as<PrimativeType>(arg)) = [&](auto& s, auto& t) {
      static auto const valid_casts = cast_closure({
        {PrimativeType::INTEGER, PrimativeType::FLOAT32},
        {PrimativeType::INTEGER, PrimativeType::FLOAT64},
        {PrimativeType::FLOAT32, PrimativeType::INTEGER},
        {PrimativeType::FLOAT64, PrimativeType::INTEGER},
        {PrimativeType::INTEGER, PrimativeType::CHAR},
        {PrimativeType::CHAR, PrimativeType::INTEGER},
        {PrimativeType::BOOL, PrimativeType::INTEGER},
        {PrimativeType::CHAR, PrimativeType::STRING}
      });
      return valid_casts.contains(std::make_pair(s.tag,t.tag));
    },
    pattern(_, _) = []{ return false; });

  if (!valid_cast) {
    throw_error_at(as_cast, "invalid cast from {} to {}",
      type_to_string(source_type.get()), type_to_string(as_cast.type.get()));
  }
  return valid_cast;
}

bool is_switchable_type(Type_Ptr type) {
  using namespace mpark::patterns;
  return match(remove_reference(type)->v)(
    pattern(as<EnumType>(_)) = []{
      return true;
    },
    pattern(as<PrimativeType>(arg)) = [](auto& primative){
      WHEN(primative.is_integer_type()) {
        return true;
      };
    },
    pattern(_) = [&]{
      return false;
    }
  );
}

void assert_has_switchable_type(Expr_Ptr expr) {
  if (!is_switchable_type(expr->meta.type)) {
    throw_error_at(expr,
      "not a switchable type (is {}), expected integral type or enum",
      type_to_string(expr->meta.type.get()));
  }
}
