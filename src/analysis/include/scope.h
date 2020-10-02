#pragma once

#include <vector>
#include <variant>
#include "types.h"
#include "ast/ast_node.h"

using SymbolName = Ast_Identifier;

struct LocalSymbolMeta {
  /* put llvm alloc stuff here */
};

struct GlobalSymbolMeta {
  /* put llvm global var stuff here */
};

struct Symbol {
  SymbolName name;
  Type_Ptr type;

  /* this may make sense just to inline into the Symbol struct */
  using Meta = std::variant<
    std::monostate,
    LocalSymbolMeta,
    GlobalSymbolMeta>;

  enum Kind {
    FUNCTION,
    GLOBAL,
    LOCAL,
    TYPE,
  } kind;

  Meta meta;

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
      auto found = std::find_if(scope->symbols.begin(), scope->symbols.end(),
        [&](auto const & symbol) {
          return symbol.name.name == name;
        });
      if (found != scope->symbols.end()) {
        return &(*found);
      }
    }
    return nullptr;
  }
};

