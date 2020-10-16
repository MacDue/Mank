#pragma once

#include <vector>

#include "scope.h"
#include "ast/ast_node.h"

struct Ast_Expression_Statement: Ast_Node {
  Expression_Ptr expression;
  bool final_expr = false;
};

struct Ast_Return_Statement: Ast_Node {
  Expression_Ptr expression;
};

struct Ast_Block: Ast_Node {
  Scope scope;
  std::vector<Statement_Ptr> statements;
  Expression_Ptr final_expr;
};

struct Ast_If_Statement: Ast_Node {
  bool has_else = false;
  Expression_Ptr cond;
  Statement_Ptr then_block, else_block;
};

struct Ast_Assign: Ast_Node {
  Expression_Ptr target, expression;
};

struct Ast_Variable_Declaration: Ast_Node {
  Type_Ptr type;
  Ast_Identifier variable;
  Expression_Ptr initializer = nullptr;
};

struct Ast_For_Loop: Ast_Node {
  Ast_Identifier loop_value;
  Type_Ptr value_type;
  Expression_Ptr start_range;
  Expression_Ptr end_range;
  Statement_Ptr body;
};

using Ast_Statement_Type = std::variant<
  Ast_Expression_Statement,
  Ast_Return_Statement,
  Ast_Block,
  Ast_If_Statement,
  Ast_Assign,
  Ast_Variable_Declaration,
  Ast_For_Loop>;

struct Ast_Statement {
  Ast_Statement_Type v;
  Ast_Statement(Ast_Statement_Type v)
    : v{std::move(v)} {}
};

