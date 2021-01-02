#pragma once

#include <type_traits>

#include "ast/ast.h"

/*
  Basically all the following does is allow me not to define all
  the before/after/visit functions for each node just the operator().

  The point of AstVisitor is to wrap around all the std::visit calls,
  and to allow you to traverse the AST without needing to do all the
  traversal explictly.

  before/after are just there for neatness
  visit is there to hide overloading operator() -- which is a little clunky
*/

template<typename TAst, typename... TAstRest>
struct AstVisitor;

/* Base visitor */
template<typename TAst>
struct AstVisitor<TAst> {
  virtual void operator()(TAst&) = 0;

protected:
  virtual void before(TAst&) {};
  virtual void after(TAst&)  {};
  virtual void visit(TAst&)  {};
};

#define with(func)                    \
  using AstVisitor<TAst>::func;       \
  using AstVisitor<TAstRest...>::func;\

/* recursion! */
template<typename TAst, typename... TAstRest>
struct AstVisitor: public AstVisitor<TAst>, public AstVisitor<TAstRest...>
{
  with(visit); with(before); with(after);
};

#undef with

struct BaseAstVisitor: public AstVisitor<
  /* Constructs */
  Ast_File,
  Ast_Function_Declaration,
  /* Statements */
  Ast_Expression_Statement,
  Ast_Return_Statement,
  Ast_Assign,
  Ast_Variable_Declaration,
  Ast_For_Loop,
  Ast_Tuple_Structural_Binding,
  Ast_Loop,
  Ast_While_Loop,
  Ast_Loop_Control,
  /* Expressions */
  Ast_Block,
  Ast_If_Expr,
  Ast_Call,
  Ast_Literal,
  Ast_Identifier,
  Ast_Unary_Operation,
  Ast_Binary_Operation,
  Ast_Field_Access,
  Ast_Array_Literal,
  Ast_Index_Access,
  Ast_Lambda,
  Ast_Macro_Identifier,
  Ast_Tuple_Literal,
  Ast_Pod_Literal,
  Ast_As_Cast
> {
  void operator()(Ast_File& file) override;
  void operator()(Ast_Function_Declaration& func) override;

  void operator()(Ast_Expression_Statement& expr_stmt) override;
  void operator()(Ast_Return_Statement& return_stmt) override;
  void operator()(Ast_Assign& assign) override;
  void operator()(Ast_Variable_Declaration& var_decl) override;
  void operator()(Ast_For_Loop& for_loop) override;
  void operator()(Ast_Tuple_Structural_Binding& binding) override;
  void operator()(Ast_Loop& loop) override;
  void operator()(Ast_While_Loop& while_loop) override;
  void operator()(Ast_Loop_Control& loop_control) override;

  void operator()(Ast_Block& ast_block) override;
  void operator()(Ast_If_Expr& if_expr) override;
  void operator()(Ast_Call& call) override;
  void operator()(Ast_Literal& literal) override;
  void operator()(Ast_Identifier& ident) override;
  void operator()(Ast_Unary_Operation& unary) override;
  void operator()(Ast_Binary_Operation& binop) override;
  void operator()(Ast_Field_Access& access) override;
  void operator()(Ast_Array_Literal& array) override;
  void operator()(Ast_Index_Access& index) override;
  void operator()(Ast_Lambda& lambda) override;
  void operator()(Ast_Macro_Identifier& macro_ident) override;
  void operator()(Ast_Tuple_Literal& tuple) override;
  void operator()(Ast_Pod_Literal& pod) override;
  void operator()(Ast_As_Cast& as_cast) override;
};
