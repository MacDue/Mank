#pragma once

#include <vector>

#include "ast/ast_node.h"
#include "primative_type.h"

struct Ast_Call: Ast_Node {
  Expression_Ptr callee;
  std::vector<Expression_Ptr> arguments;
};

struct Ast_Literal: Ast_Node {
  PrimativeTypeTag literal_type;
  std::string value;
};

struct Ast_Unary_Operation: Ast_Node {
  Expression_Ptr operand;
  Ast_Operator operation;
};

struct Ast_Binary_Operation: Ast_Node {
  Expression_Ptr left, right;
  Ast_Operator operation;
  bool parenthesised = false;
};

using Ast_Expression_Type = std::variant<
  Ast_Call,
  Ast_Literal,
  Ast_Identifier,
  Ast_Unary_Operation,
  Ast_Binary_Operation>;

struct Ast_Expression {
  Ast_Expression_Type v;
  Ast_Expression(Ast_Expression_Type v)
    : v{std::move(v)} {}
};
