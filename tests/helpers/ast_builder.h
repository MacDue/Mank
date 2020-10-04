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
  return std::make_shared<Ast_Statement>(stmt);
}

template <typename... TStmt>
Statement_Ptr make_block(TStmt && ... stmts) {
  return to_stmt_ptr(make_body(stmts...));
}


/* Statements */

// TODO

/* Expressions */

// TODO
