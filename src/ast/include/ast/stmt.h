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

using Ast_Bind = Ast_Argument;

#define AST_BINDS Ast_Tuple_Binds, Ast_Pod_Binds

struct Ast_Pod_Bind;
struct Ast_Pod_Binds {
  SourceLocation location;
  std::vector<Ast_Pod_Bind> binds;
  Type_Ptr pod_type = nullptr;
};

struct Ast_Tuple_Binds {
  SourceLocation location;
  /*
    (a, b)
    (a, {.foo})
    (a, (b,c))
  */
  std::vector<std::variant<Ast_Bind, AST_BINDS>> binds;
};

struct Ast_Pod_Bind {
  Ast_Identifier field;
  int field_index = -1;
  Ast_Identifier* bound_name = nullptr;

  /*
    .foo
    .foo/(a,b)
    .foo/{.bar}
  */
  std::variant<Ast_Bind, AST_BINDS> replacement;
};

using Ast_Binding = std::variant<Ast_Tuple_Binds, Ast_Pod_Binds>;

DEF_STMT(Ast_Structural_Binding) {
  Ast_Binding bindings;
  Expr_Ptr initializer;
};

DEF_STMT(Ast_For_Loop) {
  Type_Ptr type; // of loop variable
  Ast_Identifier loop_variable;
  Expr_Ptr start_range, end_range;
  Ast_Block body;
};

DEF_STMT(Ast_Loop) {
  Ast_Block body;
  bool may_break = false;
};

DEF_STMT(Ast_While_Loop) {
  Expr_Ptr cond;
  Ast_Block body;
};

DEF_STMT(Ast_Loop_Control) {
  enum {
    NONE,
    BREAK,
    CONTINUE
  } type = NONE;
};

// Pretty much the same as var decls
DEF_STMT(Ast_Constant_Declaration) {
  Type_Ptr type;
  Ast_Identifier constant;
  Expr_Ptr const_expression;
};

using Ast_Statement_Type = std::variant<
  Ast_Expression_Statement,
  Ast_Return_Statement,
  Ast_Assign,
  Ast_Variable_Declaration,
  Ast_Loop_Control,
  Ast_For_Loop,
  Ast_Structural_Binding,
  Ast_Loop,
  Ast_While_Loop,
  Ast_Constant_Declaration>;

class Ast_Statement {
  Ast_Statement(Ast_Statement_Type v)
    : v{std::move(v)} {}
  friend class AstContext;
public:
  Ast_Statement_Type v;
};
