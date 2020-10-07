#pragma once

#include <type_traits>

#include "ast.h"

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
  Ast_Block,
  Ast_If_Statement,
  /* Expressions */
  Ast_Call,
  Ast_Literal,
  Ast_Identifier,
  Ast_Unary_Operation,
  Ast_Binary_Operation
> {
  void operator()(Ast_File& file) override;
  void operator()(Ast_Function_Declaration& func) override;

  void operator()(Ast_Expression_Statement& expr_stmt) override;
  void operator()(Ast_Return_Statement& return_stmt) override;
  void operator()(Ast_Block& ast_block) override;
  void operator()(Ast_If_Statement& if_stmt) override;

  void operator()(Ast_Call& call) override;
  void operator()(Ast_Literal& literal) override;
  void operator()(Ast_Identifier& ident) override;
  void operator()(Ast_Unary_Operation& unary) override;
  void operator()(Ast_Binary_Operation& binop) override;
};
