#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/items.h"
#include "ast/scope.h"
#include "ast/block.h"
#include "ast/context.h"
#include "ast/generic_type.h"

/* Specialized pointers */
using Function_Ptr = SpAstPtr<Type, Ast_Function_Declaration>;
using Const_Ptr = SpAstPtr<Ast_Statement, Ast_Constant_Declaration>;
using TypeParam = SpAstPtr<Type, GenericType>;

DEF_TYPE(Ast_Function_Declaration), Ast_Node {
  bool external   :1 = false,
       c_function :1 = false,
       procedure  :1 = false,
       lambda     :1 = false,
       generic    :1 = false,
       test       :1 = false;
  Ast_Identifier identifier;
  std::vector<Ast_Argument> arguments;
  std::vector<TypeParam> type_parameters;
  Type_Ptr return_type;
  Ast_Block body;
};

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
