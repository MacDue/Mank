#pragma once

#include "ast/node.h"
#include "ast/scope.h"

struct Ast_Block: Ast_Const_Expr {
  Scope scope;
  std::vector<Statement_Ptr> statements;
  bool has_final_expr = false;

  Expression_Ptr get_final_expr() const;
};