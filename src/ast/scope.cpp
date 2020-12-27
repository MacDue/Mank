#include "ast/scope.h"

Symbol& Scope::add(Symbol symbol) {
  symbol.scope = this;
  auto& symbol_bucket = symbols[symbol.name.name]; // (create/find)
  symbol_bucket.push_back(symbol);
  return symbol_bucket.back();
}

Symbol* Scope::lookup_first(std::string const & name) {
  for (Scope* scope = this; scope != nullptr; scope = scope->parent) {
    if (scope->symbols.contains(name)) {
      return &scope->symbols.at(name).back();
    }
  }
  return nullptr;
}

void Scope::destroy_locals() {
  // TODO: Rethink symbol tables. Probably a better way.
  std::erase_if(this->symbols, [](auto& entry){
    auto& [name, symbol_bucket] = entry;
    std::erase_if(symbol_bucket,
      [](Symbol const & symbol){ return symbol.kind == Symbol::LOCAL; });
    return symbol_bucket.empty();
  });
}
