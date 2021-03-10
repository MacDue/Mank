#pragma once

#include <vector>
#include "ast/items.h"
#include "ast/block.h"
#include "ast/generic_type.h"

using TypeParam = SpAstPtr<Type, GenericType>;

// TODO: Make this an item along with consts
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

