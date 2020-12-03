#pragma once

#include "ast/defs.h"
#include "ast/ptr.h"

/* Test AST based on std::variant & pattern matching */

// All ast objs must have a self ast pointer (as raw pointers may be unsafe)
template <typename TClass, typename TSelf>
class AstSelf {
  using SelfPtr = SpAstPtr<TClass, TSelf>;
  SelfPtr self;
  friend class ContextData;
  // Lambda has to be special and override self :/
  friend class Ast_Lambda;
public:
  TSelf* get_raw_self() { return this; }

  SelfPtr operator&() const {
    // This is largely so I don't mess up!
    // If I used a raw pointer somewhere that stores the type, things will
    // go bang!
    return self;
  }
};

// Some helpers for defining new "nodes"
#define DEF_EXPR(name) struct name: Ast_Node, Ast_Expression_Node, AstSelf<Ast_Expression, name>
#define DEF_STMT(name) struct name: Ast_Node, AstSelf<Ast_Statement, name>
#define DEF_TYPE(name) struct name: AstSelf<Type, name>
