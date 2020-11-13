#pragma once

#include <vector>
#include <memory>
#include <cassert>

#include "ast/node.h"
#include "ast/block.h"
#include "ast/lambda.h"
#include "ast/construct.h"
#include "ast/primative_types.h"

struct Ast_If_Expr: Expression_Node {
  bool has_else = false;
  Expression_Ptr cond;
  Expression_Ptr then_block, else_block;
};

struct Ast_Call: Expression_Node {
  Expression_Ptr callee;
  std::vector<Expression_Ptr> arguments;
};

struct Ast_Literal: Expression_Node {
  PrimativeType::Tag literal_type;
  std::string value;

  int32_t as_int32();
  double as_float64();
  float as_float32();
  bool as_bool();

  int size_bytes();
};

struct Ast_Unary_Operation: Expression_Node {
  Expression_Ptr operand;
  Ast_Operator operation;
};

struct Ast_Binary_Operation: Expression_Node {
  Expression_Ptr left, right;
  Ast_Operator operation;
  bool parenthesised = false;
};

struct Ast_Field_Access: Expression_Node {
  Expression_Ptr object;
  Ast_Identifier field;
  int field_index = -1;
};

struct Ast_Array_Literal: Expression_Node {
  std::vector<Expression_Ptr> elements;
};

struct Ast_Index_Access: Expression_Node {
  Expression_Ptr object, index;
};

// Just want something different to make errors easier
// struct Ast_Macro_Ident: Ast_Identifier {};

using Ast_Expression_Type = std::variant<
  Ast_Block,
  Ast_If_Expr,
  Ast_Call,
  Ast_Literal,
  Ast_Identifier,
  Ast_Unary_Operation,
  Ast_Binary_Operation,
  Ast_Field_Access,
  Ast_Array_Literal,
  Ast_Index_Access,
  Ast_Lambda>;

struct Ast_Expression {
  Ast_Expression_Type v;
  Expression_Meta meta;

  template<typename Expr>
  Ast_Expression(Expr expr) {
    expr.meta = &meta;
    v = std::move(expr);
    if (std::get_if<Ast_Identifier>(&v)) {
      // ^ for some reason std::is_same_v randomly fails
      meta.value_type = Expression_Meta::LVALUE;
    }
  }

  inline bool is_lvalue() const {
    return meta.value_type == Expression_Meta::LVALUE;
  }

  inline bool is_rvalue() const {
    return !is_lvalue();
  }

  inline void set_value_type(Expression_Meta::ValueType value_type) {
    meta.value_type = value_type;
  }

  inline void inherit_value_type(Ast_Expression const & child) {
    set_value_type(child.meta.value_type);
  }
};
