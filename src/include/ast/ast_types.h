#pragma once

#include "ast/ast_node.h"
#include "primative_type.h"

struct UncheckedType {
  Ast_Identifier identifer;
};

struct PrimativeType {
  PrimativeTypeTag tag;

  PrimativeType() = default;
  PrimativeType(PrimativeTypeTag tag)
    : tag{tag} {}
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
