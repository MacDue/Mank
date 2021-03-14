#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/scope.h"
#include "ast/construct.h"

using Closure = std::vector<Symbol*>;

struct Ast_Lambda:
  Ast_Function_Declaration,
  Ast_Expression_Node<Ast_Lambda>
{
  using AstSelf<Ast_Expression, Ast_Lambda>::self;
  using AstSelf<Ast_Expression, Ast_Lambda>::operator&;
  using AstSelf<Ast_Expression, Ast_Lambda>::get_self;
  using AstSelf<Ast_Expression, Ast_Lambda>::get_raw_self;

  Closure closure;
  bool top_level_wrapper = false;

  Ast_Lambda() {
    this->lambda = true;
    this->identifier.name = "!lambda";
  }

  void generate_closure();
};

DEF_TYPE(LambdaType) {
  std::vector<Type_Ptr> argument_types;
  Type_Ptr return_type;
  SpAstPtr<Ast_Expression, Ast_Lambda> lambda;
};
