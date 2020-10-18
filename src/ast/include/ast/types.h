#pragma once

#include <string>

#include "ast/construct.h"
#include "ast/primative_types.h"

struct UncheckedType {
  Ast_Identifier identifer;
};

// (Great name! The type of the type that represents our types...)
using Type_Type = std::variant<
  UncheckedType,
  PrimativeType,
  Ast_Function_Declaration>;

struct Type {
  Type_Type v;

  Type(Type_Type v)
    : v{std::move(v)} {};
};

std::string type_to_string(Type& type);
std::string type_to_string(Type* type);

Type_Ptr extract_type_nullable(std::weak_ptr<Type> weak_type_ptr);
Type_Ptr extract_type(std::weak_ptr<Type> weak_type_ptr);
