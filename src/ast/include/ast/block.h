#pragma once

#include "ast/node.h"
#include "ast/scope.h"

DEF_EXPR(Ast_Block) {
  Scope scope;
  std::vector<Stmt_Ptr> statements;
  bool has_final_expr = false;

  Expr_Ptr get_final_expr() const;
};
