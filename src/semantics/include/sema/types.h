#pragma once

#include <utility>
#include <optional>
#include <type_traits>
#include <mpark/patterns.hpp>

#include "ast/types.h"
#include "ast/ast_builder.h"

#include "sema/type_infer.h"
#include "sema/sema_errors.h"

// The resolved type + a the type's identifier in the source (for error messages)
using TypeResolution = std::pair<Type_Ptr, std::optional<Ast_Identifier>>;
using MakeConstraint = std::optional<Infer::MakeConstraint>;

bool match_types(Type_Ptr a, Type_Ptr b,
  MakeConstraint const & make_constraint = std::nullopt,
  bool ignore_refs = true);

template <typename T>
T min_type(T a, T b) {
  using namespace mpark::patterns;
  return match(a->v, b->v)(
    pattern(as<TypeVar>(_), _) = [&]{ return a; },
    pattern(_, as<TypeVar>(_)) = [&]{ return b; },
    pattern(_, _) = [&]{ return a; }
  );
}

TypeResolution resolve_type(Scope& scope, Type_Ptr type);

Type_Ptr get_field_type(Type_Ptr type, Ast_Field_Access& access);

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
  return std::get_if<T1>(&remove_reference(type)->v);
}
