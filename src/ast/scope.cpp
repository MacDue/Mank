#include <mpark/patterns.hpp>

#include "ast/expr.h"
#include "ast/scope.h"
#include "ast/types.h"

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

Scope::PathResolution Scope::resolve_path(Ast_Path const & path) {
  // VERY wonky path resolution (will only work now)
  using namespace mpark::patterns;
  Symbol* first_lookup = lookup_first_name(path.path.at(0));

  PathResolution res;
  if (!first_lookup) return res;

  switch (first_lookup->kind){
    case Symbol::GLOBAL:
      res = first_lookup->const_value;
      break;
    case Symbol::FUNCTION:
    case Symbol::INPUT:
    case Symbol::LOCAL:
    case Symbol::TYPE:
      res = remove_reference(first_lookup->type);
    default:
      break;
  }

  bool end_of_path = false;
  for (size_t idx = 1; idx < path.path.size(); idx++) {
    auto& segment = path.path[idx];
    if (end_of_path) {
      throw_error_at(segment, "unexpected path segment");
    }
    res = match(res)(
      pattern(as<Type_Ptr>(arg)) = [&](auto& ty){
        WHEN(std::holds_alternative<EnumType>(ty->v)) {
          auto& enum_type = std::get<EnumType>(ty->v);
          // throws if enum does not have a member
          (void) enum_type.get_member(segment);
          end_of_path = true;
          return PathResolution { ty };
        };
      },
      pattern(_) = [&]{
        // TODO: better error
        throw_error_at(segment, "invalid path");
        return PathResolution{};
      }
    );
  }

  return res;
}

namespace AstHelper {
std::pair<EnumType*, EnumType::Member*> path_as_enum_member(Ast_Path& path, Scope& scope) {
  auto res = scope.resolve_path(path);
  if (auto ty = std::get_if<Type_Ptr>(&res)) {
    if (auto enum_type = std::get_if<EnumType>(&(*ty)->v)) {
      return std::make_pair(
        enum_type->get_raw_self(), &enum_type->get_member(path));
    }
  }
  throw_error_at(path, "not an enum member");
}
}
