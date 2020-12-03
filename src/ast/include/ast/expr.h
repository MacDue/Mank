#pragma once

#include <vector>
#include <memory>
#include <cassert>

#include "ast/node.h"
#include "ast/block.h"
#include "ast/lambda.h"
#include "ast/construct.h"
#include "ast/primative_types.h"

DEF_EXPR(Ast_If_Expr) {
  bool has_else = false;
  Expr_Ptr cond;
  Expr_Ptr then_block, else_block;
};

DEF_EXPR(Ast_Call) {
  Expr_Ptr callee;
  std::vector<Expr_Ptr> arguments;
};

DEF_EXPR(Ast_Literal) {
  PrimativeType::Tag literal_type;
  std::string value;

  int32_t as_int32();
  double as_float64();
  float as_float32();
  bool as_bool();

  int size_bytes();
};

DEF_EXPR(Ast_Unary_Operation) {
  Expr_Ptr operand;
  Ast_Operator operation;
};

DEF_EXPR(Ast_Binary_Operation) {
  Expr_Ptr left, right;
  Ast_Operator operation;
  bool parenthesised = false;
};

DEF_EXPR(Ast_Field_Access) {
  Expr_Ptr object;
  Ast_Identifier field;
  int field_index = -1;
};

DEF_EXPR(Ast_Expression_List) {
  std::vector<Expr_Ptr> elements;
};

struct Ast_Array_Literal: Ast_Expression_List {};

struct Ast_Tuple_Literal: Ast_Expression_List {};

DEF_EXPR(Ast_Index_Access) {
  Expr_Ptr object, index;
};

// Just want something different to make errors easier
struct Ast_Macro_Identifier: Ast_Identifier {};

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
  Ast_Lambda,
  Ast_Macro_Identifier,
  Ast_Tuple_Literal>;

class Ast_Expression {
  template<typename Expr>
  Ast_Expression(Expr expr) {
    v = std::move(expr);
    if (std::get_if<Ast_Identifier>(&v)) {
      // ^ for some reason std::is_same_v randomly fails
      meta.value_type = Expression_Meta::LVALUE;
    }
  }
  friend class AstContext;
public:
  Ast_Expression_Type v;
  Expression_Meta meta;

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
