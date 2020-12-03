#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/block.h"

DEF_STMT(Ast_Expression_Statement) {
  Expr_Ptr expression;
  bool final_expr = false;
};

DEF_STMT(Ast_Return_Statement) {
  Expr_Ptr expression;
};

DEF_STMT(Ast_Assign) {
  Expr_Ptr target, expression;
};

DEF_STMT(Ast_Variable_Declaration) {
  Type_Ptr type;
  Ast_Identifier variable;
  Expr_Ptr initializer;
};

struct TupleBinding {
  SourceLocation location;
  std::vector<std::variant<Ast_Argument, TupleBinding>> binds;
};

DEF_STMT(Ast_Tuple_Structural_Binding) {
  TupleBinding bindings;
  Expr_Ptr initializer;
};

DEF_STMT(Ast_For_Loop) {
  Type_Ptr type; // of loop variable
  Ast_Identifier loop_variable;
  Expr_Ptr start_range, end_range;
  Ast_Block body;
};

using Ast_Statement_Type = std::variant<
  Ast_Expression_Statement,
  Ast_Return_Statement,
  Ast_Assign,
  Ast_Variable_Declaration,
  Ast_For_Loop,
  Ast_Tuple_Structural_Binding>;

class Ast_Statement {
  Ast_Statement(Ast_Statement_Type v)
    : v{std::move(v)} {}
  friend class AstContext;
public:
  Ast_Statement_Type v;
};
