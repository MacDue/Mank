#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/scope.h"
#include "ast/construct.h"

struct LambdaType {
  std::vector<Type_Ptr> argument_types;
  Type_Ptr return_type;
};

using Closure = std::vector<Symbol*>;

struct Ast_Lambda: Ast_Expression_Node, Ast_Function_Declaration {
  Closure closure;

  Ast_Lambda() {
    this->lambda = true;
    this->identifier.name = "!lambda";
  }

  void generate_closure();
};
