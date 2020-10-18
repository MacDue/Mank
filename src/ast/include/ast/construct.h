#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/expr.h"
#include "ast/scope.h"

struct Ast_File: Ast_Node {
  Scope scope;
  std::string filename;
  std::vector<Function_Ptr> functions;
};

struct Ast_Function_Declaration: Ast_Node {
  bool external = false;
  bool c_function = false;
  bool procedure = false;
  Ast_Identifier identifer;
  std::vector<Ast_Argument> arguments;
  Type_Ptr return_type;
  Ast_Block body;
};
