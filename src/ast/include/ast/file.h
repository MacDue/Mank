#pragma once

#include <vector>
#include "ast/node.h"
#include "ast/scope.h"
#include "ast/context.h"

using Function_Ptr = SpAstPtr<Type, Ast_Function_Declaration>;
using Const_Ptr = SpAstPtr<Ast_Statement, Ast_Constant_Declaration>;

struct Ast_File {
  Scope scope;
  std::string filename;
  // TODO: Merge consts + functions into items
  std::vector<Function_Ptr> functions;
  std::vector<Item_Ptr> items;
  std::vector<Const_Ptr> global_consts;
  AstContext ctx; // owns EVERYTHING

  Ast_File(std::string filename)
    : filename{filename} {}

  Ast_File(Ast_File&& file) = default;
};
