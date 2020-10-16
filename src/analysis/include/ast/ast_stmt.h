#pragma once

#include <vector>

#include "ast/ast_node.h"
#include "ast/ast_block.h"

struct Ast_Expression_Statement: Ast_Node {
  Expression_Ptr expression;
  bool final_expr = false;
};

struct Ast_Return_Statement: Ast_Node {
  Expression_Ptr expression;
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
  Ast_Block body;
};

using Ast_Statement_Type = std::variant<
  Ast_Expression_Statement,
  Ast_Return_Statement,
  Ast_Assign,
  Ast_Variable_Declaration,
  Ast_For_Loop>;

struct Ast_Statement {
  Ast_Statement_Type v;
  Ast_Statement(Ast_Statement_Type v)
    : v{std::move(v)} {}
};

