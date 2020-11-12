#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/scope.h"
#include "ast/block.h"

struct Ast_File: Ast_Node {
  Scope scope;
  std::string filename;
  std::vector<Function_Ptr> functions;
  std::vector<Type_Ptr> pods;
};

struct Ast_Function_Declaration: Ast_Node {
  bool external = false,
       c_function = false,
       procedure = false,
       lambda = false;
  Ast_Identifier identifier;
  std::vector<Ast_Argument> arguments;
  Type_Ptr return_type;
  Ast_Block body;
};

struct Ast_Pod_Declaration: Ast_Node {
  Ast_Identifier identifier;
  std::vector<Ast_Argument> fields;
};
