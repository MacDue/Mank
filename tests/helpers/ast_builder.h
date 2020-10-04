#pragma once

#include <string>

#include "ast.h"

/* Constructs */

template<typename... TFunction>
Ast_File make_file(TFunction && ... function) {
  Ast_File file;
  file.filename = "<built_ast>";
  file.functions.push_back(
    std::make_shared<Type>(function)...);
  return file;
}

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

template <typename... TStmt>
Ast_Block make_body(TStmt && ... stmts) {
  Ast_Block block;
  block.statements = std::vector<Statement_Ptr>{ stmts... };
  return block;
}

template <typename TStmt>
Statement_Ptr to_stmt_ptr(TStmt && stmt) {
  stmt.location = {};
  return std::make_shared<Ast_Statement>(stmt);
}

template <typename... TStmt>
Statement_Ptr make_block(TStmt && ... stmts) {
  return to_stmt_ptr(make_body(stmts...));
}


/* Statements */

inline Statement_Ptr make_expr_stmt(Expression_Ptr expr) {
  Ast_Expression_Statement expr_stmt;
  expr_stmt.expression = expr;
  return to_stmt_ptr(expr_stmt);
}

// TODO

/* Expressions */

template <typename TExpr>
Expression_Ptr to_expr_ptr(TExpr && expr) {
  expr.location = {};
  return std::make_shared<Ast_Expression>(expr);
}

inline Expression_Ptr make_literal(PrimativeTypeTag type, std::string value) {
  Ast_Literal literal;
  literal.literal_type = type;
  literal.value = value;
  return to_expr_ptr(literal);
}

inline Expression_Ptr make_string(std::string value) {
  return make_literal(PrimativeTypeTag::STRING, value);
}

inline Expression_Ptr make_ident(std::string name) {
  Ast_Identifier ident;
  ident.name = name;
  return to_expr_ptr(ident);
}

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

// TODO
