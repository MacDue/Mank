#pragma once

#include <memory>

#include "ast/node.h"

struct ContextData;
class AstContext {
  std::unique_ptr<ContextData> data;

  Type_Ptr add_type(Type type);
  Stmt_Ptr add_stmt(Ast_Statement type);
  Expr_Ptr add_expr(Ast_Expression type);
public:
  AstContext();

  template <typename T>
  Type_Ptr new_type(T type) {
    return add_type(Type(type));
  }

  template <typename T>
  Stmt_Ptr new_stmt(T stmt) {
    return add_stmt(Ast_Statement(stmt));
  }

  template <typename T>
  Expr_Ptr new_expr(T expr) {
    return add_expr(Ast_Expression(expr));
  }

  ~AstContext();
};
