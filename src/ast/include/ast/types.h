#pragma once

#include <string>

#include "ast/construct.h"
#include "ast/array_type.h"
#include "ast/lambda.h"
#include "ast/primative_types.h"

struct TypeVar {
  uint32_t id;
  // OR
  enum {
    TYPE_VAR = 0,
    NUMERIC, // any int or float
    INTEGER, // any int
  } special_constraint = TYPE_VAR;

  TypeVar() {
    static auto next_id = 0;
    id = next_id++;
  }

  TypeVar(auto constraint): special_constraint{constraint} {}

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
