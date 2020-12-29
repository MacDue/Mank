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
      pattern(as<Ast_Pod_Declaration>(arg), as<Ast_Pod_Declaration>(arg)) =
        [](auto const & a, auto const & b) {
          return a.identifier.name == b.identifier.name;
        },
      pattern(as<FixedSizeArrayType>(arg), as<FixedSizeArrayType>(arg)) =
        [&](auto const & a, auto const & b) {
           // refs can't appear in arrays
          return a.size == b.size
            && match_types(a.element_type, b.element_type, make_constraint);
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

#define TYPE_MUST_BE_KNOWN "type must be known here! Maybe add a type annotation?"

Type_Ptr get_field_type(
  Type_Ptr type,
  Ast_Field_Access& access,
  ResolvedPodInfoMap const & pod_info
) {
  using namespace mpark::patterns;
  if (type) {
    auto access_type = match(remove_reference(type)->v)(
      pattern(as<Ast_Pod_Declaration>(arg)) = [&](auto& pod_type) {
        auto [field_type, field_index] =
          pod_info.at(pod_type.identifier.name).get_field_or_fail(access.field);
        access.field_index = field_index;
        access.get_meta().value_type = access.object->meta.value_type;
        // expr.inherit_value_type(*access.object);
        return field_type;
      },
      // FIXME: Hardcoded .length
      pattern(as<FixedSizeArrayType>(_)) = [&]{
        WHEN(access.field.name == "length") {
          return PrimativeType::int_ty();
        };
      },
      pattern(as<PrimativeType>(arg)) = [&](auto& primative_type) {
        WHEN(primative_type.is_string_type() && access.field.name == "length") {
          return PrimativeType::int_ty();
        };
      },
      pattern(as<TypeVar>(_)) = [&]() -> Type_Ptr {
        throw_sema_error_at(access.object, TYPE_MUST_BE_KNOWN);
        return nullptr;
      },
      pattern(_) = []() -> Type_Ptr { return nullptr; });
    if (access_type) {
      return access_type;
    }
  }
  throw_sema_error_at(access.object, "not a pod type");
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
        return PrimativeType::get(PrimativeType::CHAR);
      };
    },
    pattern(_) = [&]() -> Type_Ptr {
      throw_sema_error_at(access.object, "not an indexable type (is {})",
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
      throw_sema_error_at(index_access.object, TYPE_MUST_BE_KNOWN);
    }
    // assert(array_type && "should be an array type");
    // FIXME: Will break with more integer types
    auto int_index = std::get<int32_t>(*index_value);
    if (int_index < 0 || static_cast<size_t>(int_index) >= array_type->size) {
      throw_sema_error_at(index_access.index, "out of bounds index");
    }
  }
}
