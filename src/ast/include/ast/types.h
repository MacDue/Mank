#pragma once

#include <string>

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
public:
  Type_Type v;
};

std::string type_to_string(Type const & type);
std::string type_to_string(Type const * type);
