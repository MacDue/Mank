#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/scope.h"
#include "ast/construct.h"

DEF_TYPE(LambdaType) {
  std::vector<Type_Ptr> argument_types;
  Type_Ptr return_type;
};

using Closure = std::vector<Symbol*>;

struct Ast_Lambda:
  Ast_Function_Declaration,
  Ast_Expression_Node<Ast_Lambda>
{
  using AstSelf<Ast_Expression, Ast_Lambda>::self;
  Closure closure;
  bool top_level_wrapper = false;

  Ast_Lambda() {
    this->lambda = true;
    this->identifier.name = "!lambda";
  }

  void generate_closure();
};
