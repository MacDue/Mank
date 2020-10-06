#pragma once

#include "ast.h"

struct AstVisitor {
  using type = void;
  /* Constructs */
  virtual void operator()(Ast_File& file);
  virtual void operator()(Ast_Function_Declaration& func);

  /* Statements */
  virtual void operator()(Ast_Expression_Statement& expr_stmt);
  virtual void operator()(Ast_Return_Statement& return_stmt);
  virtual void operator()(Ast_Block& ast_block);
  virtual void operator()(Ast_If_Statement& if_stmt);

  /* Expressions */
  virtual void operator()(Ast_Call& call);
  virtual void operator()(Ast_Literal& literal);
  virtual void operator()(Ast_Identifier& ident);
  virtual void operator()(Ast_Unary_Operation& unary);
  virtual void operator()(Ast_Binary_Operation& binop);
};
