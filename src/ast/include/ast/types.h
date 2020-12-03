#pragma once

#include <string>

#include "ast/lambda.h"
#include "ast/type_var.h"
#include "ast/construct.h"
#include "ast/array_type.h"
#include "ast/tuple_type.h"
#include "ast/primative_types.h"

struct UncheckedType {
  Ast_Identifier identifier;
};

struct ReferenceType {
  Type_Ptr references;
};

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

struct Type {
  Type_Type v;

  Type(Type_Type v)
    : v{std::move(v)} {};
};

std::string type_to_string(Type const & type);
std::string type_to_string(Type const * type);

Type_Ptr extract_type_nullable(std::weak_ptr<Type> weak_type_ptr);
Type_Ptr extract_type(std::weak_ptr<Type> weak_type_ptr);
