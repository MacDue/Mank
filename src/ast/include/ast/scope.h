#pragma once

#include <memory>
#include <vector>
#include <unordered_map>

#include "ast/node.h"
#include "ast/common_ptrs.h"
#include "ast/user_type_info.h"

using SymbolName = Ast_Identifier;

struct SymbolMeta {
  virtual ~SymbolMeta() {};
};

struct Scope;

struct Symbol {
  SymbolName name;

  Type_Ptr type;
  Expr_Ptr const_value; // for globals

  enum Kind {
    FUNCTION,
    GLOBAL,
    INPUT,
    LOCAL,
    TYPE,
  } kind;

  /* only shared as otherwise the Ast_Expression would be a pita to deal with */
  std::shared_ptr<SymbolMeta> meta;

  struct Scope* scope = nullptr; // set when added to a scope

  inline bool is_local() {
    return kind == INPUT || kind == LOCAL;
  }

  inline bool is_global() {
    return kind == GLOBAL;
  }

  Symbol(SymbolName name, Type_Ptr type, Kind kind)
    : name{name}, type{type}, kind{kind} {};
};

class Scope {
  UserTypes::TypeMap user_types_info;
  std::unordered_map<std::string, std::vector<Symbol>> symbols;
  Scope* parent = nullptr;
  int level = 0;
public:
  inline Scope* get_parent() { return parent; }
  inline int get_level() { return level; }

  inline UserTypes::TypeMap& get_type_info() {
    return user_types_info;
  }

  inline void set_parent(Scope& parent) {
    this->parent = &parent;
    this->level = parent.level + 1;
  }

  Symbol& add(Symbol symbol);

  inline Symbol* lookup_first_name(SymbolName const & name) {
    return lookup_first(name.name);
  }

  Symbol* lookup_first(std::string const & name);

  void destroy_locals();

  using PathResolution = std::variant<Type_Ptr, Expr_Ptr>;

  PathResolution resolve_path(Ast_Path const & path);
};
