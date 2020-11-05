#pragma once

#include <utility>
#include <optional>
#include <type_traits>

#include "ast/types.h"
#include "sema/sema_errors.h"

#define make_primative_type(type_tag) \
  inline static auto type_tag = std::make_shared<Type>(PrimativeType(PrimativeType::Tag::type_tag))

namespace Primative {
  make_primative_type(FLOAT32);
  make_primative_type(FLOAT64);
  make_primative_type(INTEGER);
  make_primative_type(STRING);
  make_primative_type(BOOL);
};

// The resolved type + a the type's identifier in the source (for error messages)
using TypeResolution = std::pair<Type_Ptr, std::optional<Ast_Identifier>>;

template <typename T>
T remove_reference(T type) {
  if (!type) return type;
  if (auto ref_type = std::get_if<ReferenceType>(&type->v)) {
    if constexpr (std::is_pointer_v<T>) {
      type = ref_type->references.get();
    } else {
      type = ref_type->references;
    }
  }
  return type;
}

inline bool is_reference_type(Type const * type) {
  return remove_reference(type) != type;
}

bool match_types(Type const * a, Type const * b);

TypeResolution resolve_type(Scope& scope, Type_Ptr type);

template<typename T>
static void resolve_type_or_fail(Scope& scope, Type_Ptr& to_resolve, T error_format) {
  auto [ resolved_type, type_slot ] = resolve_type(scope, to_resolve);
  if (resolved_type) {
    to_resolve = resolved_type;
  } else if (type_slot) {
    throw_sema_error_at(*type_slot, error_format);
  } else {
    IMPOSSIBLE();
  }
}

template<typename T1>
T1* get_if_dereferenced_type(Type_Ptr& type) {
  return std::get_if<T1>(&remove_reference(type.get())->v);
}
