#pragma once

#include <string>
#include <type_traits>

#include "ast/lambda.h"
#include "ast/type_var.h"
#include "ast/construct.h"
#include "ast/array_type.h"
#include "ast/tuple_type.h"
#include "ast/primative_types.h"

DEF_TYPE(UncheckedType) {
  Ast_Identifier identifier;
  UncheckedType(Ast_Identifier identifier)
    : identifier{identifier} {}
};

DEF_TYPE(ReferenceType) {
  Type_Ptr references;
};

template <typename T>
T remove_reference(T type) {
  static_assert(std::is_same_v<std::decay_t<T>, Type_Ptr>);
  if (!type) return type;
  if (auto ref_type = std::get_if<ReferenceType>(&type->v)) {
    return ref_type->references;
  }
  return T(nullptr);
}

inline bool is_reference_type(Type_Ptr const type) {
  return remove_reference(type) != type;
}

// (Great name! The type of the type that represents our types...)
using Type_Type = std::variant<
  TypeVar,
  TupleType,
  UncheckedType,
  PrimativeType,
  Ast_Function_Declaration,
  Ast_Pod_Declaration,
  FixedSizeArrayType,
  ReferenceType,
  LambdaType,
  TypeFieldConstraint>;

class Type {
  Type(Type_Type v)
    : v{std::move(v)} {};
  friend class AstContext;
  friend class PrimativeType; // to allow static primatives
public:
  Type_Type v;

  static Type_Ptr get_primative(PrimativeType::Tag);
};

std::string type_to_string(Type const & type);
std::string type_to_string(Type const * type);
