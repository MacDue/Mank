#pragma once

#include <utility>
#include <optional>
#include <type_traits>
#include <mpark/patterns.hpp>

#include "ast/types.h"
#include "ast/ast_builder.h"

#include "sema/type_infer.h"
#include "errors/compiler_errors.h"

// The resolved type + a the type's identifier in the source (for error messages)
using TypeResolution = std::pair<Type_Ptr, std::optional<Ast_Identifier>>;
using MakeConstraint = std::optional<Infer::MakeConstraint>;

bool match_types(Type_Ptr a, Type_Ptr b,
  MakeConstraint const & make_constraint = std::nullopt,
  bool ignore_refs = true);

inline bool is_tvar(Type_Ptr type) {
  if (!type) return false;
  // Should ref TVar = TVar????
  return std::holds_alternative<TypeVar>(remove_reference(type)->v);
}

template <typename T>
T min_type(T a, T b) {
  using namespace mpark::patterns;
  // Reutrn the opposite as there's a chance it's non-null & a tvar.
  if (!a) return b;
  if (!b) return a;
  return match(a->v, b->v)(
    pattern(as<TypeVar>(_), _) = [&]{ return a; },
    pattern(_, as<TypeVar>(_)) = [&]{ return b; },
    pattern(_, _) = [&]{ return a; }
  );
}

// Used to resolve type aliases (that could be infinte types)
using TypeStarts = std::optional<std::set<Ast_Identifier>>;

TypeResolution resolve_type(Scope& scope, Type_Ptr type,
  TypeStarts starting_points = std::nullopt);

Type_Ptr get_field_type(Ast_Field_Access& access);

Type_Ptr get_field_type(TypeFieldConstraint& field_constraint);

Type_Ptr get_field_type(
  Ast_Pod_Bind& pod_bind, Expr_Ptr init, Type_Ptr init_type);

Type_Ptr get_element_type(Type_Ptr type, Ast_Index_Access& access);

bool validate_type_cast(Type_Ptr type, Ast_As_Cast& as_cast);

bool is_switchable_type(Type_Ptr type);

void assert_has_switchable_type(Expr_Ptr expr);

template<typename T>
void resolve_type_or_fail(
  Scope& scope, Type_Ptr& to_resolve, T error_format, TypeStarts starting_points = std::nullopt
) {
  auto [ resolved_type, type_slot ] = resolve_type(scope, to_resolve, starting_points);
  if (resolved_type) {
    to_resolve = resolved_type;
  } else if (type_slot) {
    throw_error_at(*type_slot, error_format);
  } else {
    IMPOSSIBLE();
  }
}

template<typename T1>
T1* get_if_dereferenced_type(Type_Ptr& type) {
  return std::get_if<T1>(&remove_reference(type)->v);
}

void static_check_array_bounds(Ast_Index_Access& index_access, bool allow_missing_types = false);
