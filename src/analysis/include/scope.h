#pragma once

#include <vector>
#include <memory>
#include "types.h"
#include "ast/ast_node.h"

using SymbolName = Ast_Identifier;

struct SymbolMeta {
  virtual ~SymbolMeta() {};
};

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

  inline bool is_local() {
    return kind == INPUT || kind == LOCAL;
  }

  /* transitively -- as the AST shared (which holds scope nodes) -- this really _should_ be unique */
  std::shared_ptr<SymbolMeta> meta;
  Symbol(SymbolName name, Type_Ptr type, Kind kind)
    : name{name}, type{type}, kind{kind} {};
};

struct Scope {
  Scope* parent = nullptr;
  std::vector<Symbol> symbols;

  Symbol* lookup_first_name(SymbolName const & name) {
    return lookup_first(name.name);
  }

  Symbol* lookup_first(std::string_view name) {
    for (Scope* scope = this; scope != nullptr; scope = scope->parent) {
      auto found = std::find_if(scope->symbols.rbegin(), scope->symbols.rend(),
        [&](auto const & symbol) {
          return symbol.name.name == name;
        });
      if (found != scope->symbols.rend()) {
        return &(*found);
      }
    }
    return nullptr;
  }

  // TODO: Rethink symbol tables. Probably a better way.
  void destroy_locals() {
    this->symbols.erase(
      std::remove_if(this->symbols.begin(), this->symbols.end(),
      [](auto const & symbol){
        return symbol.kind == Symbol::LOCAL;
      }), this->symbols.end());
  }
};

