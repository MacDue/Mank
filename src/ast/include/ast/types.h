#pragma once

#include <string>

#include "ast/construct.h"
#include "ast/array_type.h"
#include "ast/lambda.h"
#include "ast/primative_types.h"

struct TypeVar {
  int32_t id;
  // OR
  enum Constraint {
    NUMERIC = -1, // any int or float
    INTEGER = -2, // any int
  };

  inline static const Constraint Constraints[] = { NUMERIC, INTEGER };

  TypeVar() {
    static auto next_id = 0;
    id = next_id++;
  }

  TypeVar(Constraint constraint)
    : id{static_cast<int32_t>(constraint)} {}

  inline bool special() const { return id < 0; }
  inline bool operator <(TypeVar const & other) const {
    return this->id < other.id;
  }

  static Type_Ptr integer();
  static Type_Ptr numeric();
};

struct UncheckedType {
  Ast_Identifier identifier;
};

struct ReferenceType {
  Type_Ptr references;
};

// (Great name! The type of the type that represents our types...)
using Type_Type = std::variant<
  TypeVar,
  UncheckedType,
  PrimativeType,
  Ast_Function_Declaration,
  Ast_Pod_Declaration,
  FixedSizeArrayType,
  ReferenceType,
  LambdaType>;

struct Type {
  Type_Type v;

  Type(Type_Type v)
    : v{std::move(v)} {};
};

std::string type_to_string(Type const & type);
std::string type_to_string(Type const * type);

Type_Ptr extract_type_nullable(std::weak_ptr<Type> weak_type_ptr);
Type_Ptr extract_type(std::weak_ptr<Type> weak_type_ptr);
