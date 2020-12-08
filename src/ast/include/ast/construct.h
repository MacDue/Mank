#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/scope.h"
#include "ast/block.h"
#include "ast/context.h"

DEF_TYPE(Ast_Function_Declaration), Ast_Node {
  bool external = false,
       c_function = false,
       procedure = false,
       lambda = false;
  Ast_Identifier identifier;
  std::vector<Ast_Argument> arguments;
  Type_Ptr return_type;
  Ast_Block body;

  // Tvars in use in this function, used to detect incomplete inference.
  std::set<TypeVar> active_tvars;
};

DEF_TYPE(Ast_Pod_Declaration), Ast_Node {
  Ast_Identifier identifier;
  std::vector<Ast_Argument> fields;
};

using Function_Ptr = SpAstPtr<Type, Ast_Function_Declaration>;
using Pod_Ptr = SpAstPtr<Type, Ast_Pod_Declaration>;

struct Ast_File {
  Scope scope;
  std::string filename;
  std::vector<Function_Ptr> functions;
  std::vector<Pod_Ptr> pods;
  AstContext ctx; // owns EVERYTHING

  Ast_File(std::string filename)
    : filename{filename} {}

  Ast_File(Ast_File&& file) = default;
};
