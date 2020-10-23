#include "ast/scope.h"

Symbol& Scope::add(Symbol symbol) {
  symbol.scope = this;
  symbols.emplace_back(symbol);
  return symbols.back();
}

Symbol* Scope::lookup_first(std::string_view name) {
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

void Scope::destroy_locals() {
  // TODO: Rethink symbol tables. Probably a better way.
  this->symbols.erase(
    std::remove_if(this->symbols.begin(), this->symbols.end(),
    [](auto const & symbol){
      return symbol.kind == Symbol::LOCAL;
    }), this->symbols.end());
}
