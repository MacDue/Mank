#pragma once

#include <vector>
#include <memory>

#include "ast/node.h"

using SymbolName = Ast_Identifier;

struct SymbolMeta {
  virtual ~SymbolMeta() {};
};

struct Scope;

struct Symbol {
  SymbolName name;
  Type_Ptr type;

  enum Kind {
    FUNCTION,
    GLOBAL,
    INPUT,
    LOCAL,
    TYPE,
  } kind;

  /* transitively -- as the AST shared (which holds scope nodes) -- this really _should_ be unique */
  std::shared_ptr<SymbolMeta> meta;

  struct Scope* scope = nullptr; // set when added to a scope

  inline bool is_local() {
    return kind == INPUT || kind == LOCAL;
  }

  Symbol(SymbolName name, Type_Ptr type, Kind kind)
    : name{name}, type{type}, kind{kind} {};
};

class Scope {
  std::vector<Symbol> symbols;
public:
  Scope* parent = nullptr;

  Symbol& add(Symbol symbol);

  inline Symbol* lookup_first_name(SymbolName const & name) {
    return lookup_first(name.name);
  }

  Symbol* lookup_first(std::string_view name);

  void destroy_locals();
};
