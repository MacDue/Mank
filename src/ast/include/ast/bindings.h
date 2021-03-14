#pragma once

#include "ast/node.h"

using Ast_Bind = Ast_Argument;

#define AST_BINDS Ast_Tuple_Binds, Ast_Pod_Binds

struct Ast_Pod_Bind;
struct Ast_Pod_Binds {
  SourceLocation location;
  std::vector<Ast_Pod_Bind> binds;
  Type_Ptr pod_type = nullptr;
};

struct Ast_Tuple_Binds {
  SourceLocation location;
  /*
    (a, b)
    (a, {.foo})
    (a, (b,c))
  */
  std::vector<std::variant<Ast_Bind, AST_BINDS>> binds;
};

struct Ast_Pod_Bind {
  Ast_Identifier field;
  int field_index = -1;
  Ast_Identifier* bound_name = nullptr;

  /*
    .foo
    .foo/(a,b)
    .foo/{.bar}
  */
  std::variant<Ast_Bind, AST_BINDS> replacement;
};

using Ast_Binding = std::variant<Ast_Tuple_Binds, Ast_Pod_Binds>;
