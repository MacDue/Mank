#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/block.h"

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

struct TupleBinding {
  SourceLocation location;
  std::vector<std::variant<Ast_Argument, TupleBinding>> binds;
};

struct Ast_Tuple_Structural_Binding: Ast_Node {
  TupleBinding bindings;
  Expression_Ptr initializer;
};
using Ast_Sad_Binding = Ast_Tuple_Structural_Binding;

struct Ast_For_Loop: Ast_Node {
  Type_Ptr type; // of loop variable
  Ast_Identifier loop_variable;
  Expression_Ptr start_range;
  Expression_Ptr end_range;
  Ast_Block body;
};

using Ast_Statement_Type = std::variant<
  Ast_Expression_Statement,
  Ast_Return_Statement,
  Ast_Assign,
  Ast_Variable_Declaration,
  Ast_For_Loop,
  Ast_Tuple_Structural_Binding>;

struct Ast_Statement {
  Ast_Statement_Type v;
  Ast_Statement(Ast_Statement_Type v)
    : v{std::move(v)} {}
};
