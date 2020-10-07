#pragma once

#include <vector>
#include <memory>

#include "ast/ast_node.h"

struct Ast_Call: Ast_Const_Expr {
  Expression_Ptr callee;
  std::vector<Expression_Ptr> arguments;
};

struct Ast_Literal: Ast_Const_Expr {
  PrimativeTypeTag literal_type;
  std::string value;

  Ast_Literal(SourceLocation location,
    std::string value, PrimativeTypeTag type);

  int32_t as_int32();
  double as_float64();
  float as_float32();
  bool as_bool();

  int size_bytes();
};

struct Ast_Unary_Operation: Ast_Const_Expr {
  Expression_Ptr operand;
  Ast_Operator operation;
};

struct Ast_Binary_Operation: Ast_Const_Expr {
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
  /*
    The expression must not own it's resolved type or it can create cycles!

    for example in a recursive function:

    fun wee: i32 {
      return wee(); # the callee type will be `fun wee: i32``
                    # that ast node is within wee so we have a cycle.
    }

    I _think_ all will be owned by something else:
      - Primatives (static data)
      - Functions (owned by function node)
      - More complex types will probably be nodes within the AST already
  */
  std::weak_ptr<Type> type;
  Ast_Expression_Type v;
  Ast_Expression(Ast_Expression_Type v)
    : v{std::move(v)} {}

  inline PrimativeValue const_eval_unary(Ast_Operator op) {
    return std::visit([&](auto operand){
      return operand.const_eval_unary(op);
    }, v);
  }

  inline PrimativeValue const_eval_binary(Ast_Operator op, Ast_Expression& rhs) {
    return std::visit([&](auto lhs, auto rhs){
      return lhs.const_eval_binary(op, rhs);
    }, v, rhs.v);
  }
};
