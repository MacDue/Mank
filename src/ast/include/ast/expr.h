#pragma once

#include <vector>
#include <memory>
#include <cassert>

#include "ast/node.h"
#include "ast/block.h"
#include "ast/primative_types.h"

struct Ast_If_Expr: Ast_Expression_Node {
  bool has_else = false;
  Expression_Ptr cond;
  Expression_Ptr then_block, else_block;
};

struct Ast_Call: Ast_Expression_Node {
  Expression_Ptr callee;
  std::vector<Expression_Ptr> arguments;
};

struct Ast_Literal: Ast_Expression_Node {
  PrimativeType::Tag literal_type;
  std::string value;

  int32_t as_int32();
  double as_float64();
  float as_float32();
  bool as_bool();

  int size_bytes();
};

struct Ast_Unary_Operation: Ast_Expression_Node {
  Expression_Ptr operand;
  Ast_Operator operation;
};

struct Ast_Binary_Operation: Ast_Expression_Node {
  Expression_Ptr left, right;
  Ast_Operator operation;
  bool parenthesised = false;
};

struct Ast_Field_Access: Ast_Expression_Node {
  Expression_Ptr object;
  Ast_Identifier field;
  int field_index = -1;
};

using Ast_Expression_Type = std::variant<
  Ast_Block,
  Ast_If_Expr,
  Ast_Call,
  Ast_Literal,
  Ast_Identifier,
  Ast_Unary_Operation,
  Ast_Binary_Operation,
  Ast_Field_Access>;

struct Ast_Expression {
  Ast_Expression_Type v;
  Expression_Meta meta;

  template<typename Expr>
  Ast_Expression(Expr expr) {
    expr.meta = &meta;
    v = std::move(expr);
  }
};
