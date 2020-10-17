#pragma once

#include <string>

#include "ast.h"

/* Builders */

template <typename TStmt>
Statement_Ptr to_stmt_ptr(TStmt && stmt) {
  stmt.location = {};
  return std::make_shared<Ast_Statement>(stmt);
}

template <typename TExpr>
Expression_Ptr to_expr_ptr(TExpr && expr) {
  expr.location = {};
  return std::make_shared<Ast_Expression>(expr);
}

/* -- Constructs -- */

template<typename... TFunction>
Ast_File make_file(TFunction && ... function) {
  Ast_File file;
  file.filename = "<built_ast>";
  file.functions.push_back(
    std::make_shared<Type>(function)...);
  return file;
}

/* Args */

inline Ast_Argument make_argument(Type_Ptr type, std::string name) {
  return Ast_Argument {
      .type = type,
      .name = Ast_Identifier(name)
  };
}

template<typename... TArgs>
std::vector<Ast_Argument> make_args(TArgs && ... args) {
  return std::vector<Ast_Argument>{ args... };
}

/* Types */

inline Type_Ptr make_unchecked_type(std::string type_name) {
  UncheckedType unchecked;
  unchecked.identifer.name = type_name;
  return std::make_shared<Type>(unchecked);
}

/* Functions */

inline Ast_Function_Declaration make_function(
  bool procedure,
  Type_Ptr return_type,
  std::string name,
  std::vector<Ast_Argument>&& args,
  Ast_Block&& body
) {
  Ast_Function_Declaration function;
  function.identifer.name = name;
  function.procedure = procedure;
  function.return_type = return_type;
  function.arguments = args;
  function.body = body;
  return function;
}

inline Ast_Function_Declaration make_function(
  Type_Ptr return_type,
  std::string name,
  std::vector<Ast_Argument>&& args,
  Ast_Block&& body
) {
  return make_function(false, return_type, name, std::move(args), std::move(body));
}

inline Ast_Function_Declaration make_procedure(
  std::string name,
  std::vector<Ast_Argument>&& args,
  Ast_Block&& body
) {
  return make_function(true, nullptr, name, std::move(args), std::move(body));
}

inline Ast_Function_Declaration make_function(
  Type_Ptr return_type,
  std::string name,
  Ast_Block&& body
) {
  return make_function(return_type, name, {}, std::move(body));
}

inline Ast_Function_Declaration make_procedure(
  std::string name,
  Ast_Block&& body
) {
  return make_procedure(name, {}, std::move(body));
}

/* Blocks */

template <typename... TStmt>
Ast_Block make_body(bool has_final_expr, TStmt && ... stmts) {
  Ast_Block block;
  block.statements = std::vector<Statement_Ptr>{ stmts... };
  block.has_final_expr = has_final_expr;
  return block;
}

template <typename... TStmt>
Expression_Ptr make_block(bool has_final_expr, TStmt && ... stmts) {
  return to_expr_ptr(make_body(has_final_expr, stmts...));
}

template <typename... TStmt>
Ast_Block make_stmt_body(TStmt && ... stmts) {
  return make_body(false, stmts...);
}

template <typename... TStmt>
Expression_Ptr make_stmt_block(TStmt && ... stmts) {
  return make_block(false, stmts...);
}

/* -- Expressions -- */

/* If expressions */

inline Expression_Ptr make_if(
  Expression_Ptr cond, Expression_Ptr then_block, Expression_Ptr else_block = nullptr
) {
  Ast_If_Expr if_stmt;
  if_stmt.cond = cond;
  if_stmt.then_block = then_block;
  if (else_block) {
    if_stmt.has_else = true;
    if_stmt.else_block = else_block;
  }
  return to_expr_ptr(if_stmt);
}

/* Literals */

inline Expression_Ptr make_literal(PrimativeTypeTag type, std::string value) {
  Ast_Literal literal({}, value, type);
  return to_expr_ptr(literal);
}

inline Expression_Ptr make_string(std::string value) {
  return make_literal(PrimativeTypeTag::STRING, value);
}

inline Expression_Ptr make_integer(int value) {
  return make_literal(PrimativeTypeTag::INTEGER, std::to_string(value));
}

inline Expression_Ptr make_float64(double value) {
  return make_literal(PrimativeTypeTag::FLOAT64, std::to_string(value));
}

inline Expression_Ptr make_boolean(bool value) {
  return make_literal(PrimativeTypeTag::BOOL, value ? "true" : "false");
}

/* Idents */

inline Expression_Ptr make_ident(std::string name) {
  Ast_Identifier ident;
  ident.name = name;
  return to_expr_ptr(ident);
}

/* Calls */

template <typename... TArgs>
Expression_Ptr make_call(Expression_Ptr callee, TArgs && ... args) {
  Ast_Call call;
  call.callee = callee;
  call.arguments = std::vector<Expression_Ptr> { args ... };
  return to_expr_ptr(call);
}

template <typename... TArgs>
Expression_Ptr make_call(std::string callee, TArgs && ... args) {
  return make_call(make_ident(callee), args...);
}

/* Unary */

inline Expression_Ptr make_unary(Ast_Operator op, Expression_Ptr operand) {
  Ast_Unary_Operation unary;
  unary.operation = op;
  unary.operand = operand;
  return to_expr_ptr(unary);
}

/* Binary */

inline Expression_Ptr make_binary(
  Ast_Operator op, Expression_Ptr left, Expression_Ptr right
) {
  Ast_Binary_Operation binop;
  binop.operation = op;
  binop.left = left;
  binop.right = right;
  return to_expr_ptr(binop);
}

/* -- Statements -- */

/* Expression statement */

inline Statement_Ptr make_expr_stmt(Expression_Ptr expr) {
  Ast_Expression_Statement expr_stmt;
  expr_stmt.expression = expr;
  return to_stmt_ptr(expr_stmt);
}

/* Return statement */

inline Statement_Ptr make_return(Expression_Ptr value) {
  Ast_Return_Statement return_stmt;
  return_stmt.expression = value;
  return to_stmt_ptr(return_stmt);
}

/* If statement */

inline Statement_Ptr make_if_stmt(
  Expression_Ptr cond, Expression_Ptr then_block, Expression_Ptr else_block = nullptr
) {
  return make_expr_stmt(make_if(cond, then_block, else_block));
}

/* Variable declarations */

inline Statement_Ptr make_var_decl(
  std::string name,
  Type_Ptr type = nullptr,
  Expression_Ptr initializer = nullptr
) {
  Ast_Variable_Declaration var_decl;
  var_decl.type = type;
  var_decl.variable.name = name;
  var_decl.initializer = initializer;
  return to_stmt_ptr(var_decl);
}

inline Statement_Ptr make_var_decl(
  std::string name,
  Expression_Ptr initializer
) {
  return make_var_decl(name, nullptr, initializer);
}

/* Assignment statements */

inline Statement_Ptr make_assignment(
  Expression_Ptr target, Expression_Ptr value
) {
  Ast_Assign assign;
  assign.target = target;
  assign.expression = value;
  return to_stmt_ptr(assign);
}

/* Types */

inline Type_Ptr make_type(std::string name) {
  UncheckedType type;
  type.identifer.name = name;
  return std::make_shared<Type>(type);
}

/* Test helpers */

inline Ast_File wrap_stmt(Statement_Ptr stmt, bool has_final_expr = false) {
  return make_file(make_procedure("test", make_body(has_final_expr, stmt)));
}

inline Ast_File wrap_expr(Expression_Ptr expr, bool final_expr = false) {
  return wrap_stmt(make_expr_stmt(expr), final_expr);
}

inline Ast_File wrap_final_expr(Expression_Ptr expr) {
  return wrap_expr(expr, true);
}
