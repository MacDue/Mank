#pragma once

#include "ast/node.h"
#include <unordered_map>

bool operator==(Ast_Identifier const & lhs, Ast_Identifier const & rhs) {
  return lhs.name == rhs.name;
}

template<>
struct std::hash<Ast_Identifier> {
  std::size_t operator()(Ast_Identifier const & ident) const noexcept {
    return std::hash<std::string>{}(ident.name);
  }
};
