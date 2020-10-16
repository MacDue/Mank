#pragma once

#include "scope.h"
#include "ast/ast_node.h"

struct Ast_Block: Ast_Const_Expr {
  Scope scope;
  std::vector<Statement_Ptr> statements;
  Expression_Ptr final_expr;
};
