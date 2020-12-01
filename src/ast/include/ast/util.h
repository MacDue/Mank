#pragma once

#include "ast/node.h"
#include "ast/source_location.h"
#include "ast/is_abstract_ast.h"

namespace AstHelper {

template<typename TAst>
SourceLocation extract_location(TAst const & ast) {
  if constexpr (is_abstract_ast<TAst>::value) {
    return std::visit([&](auto const & ast) {
      return ast.location;
    }, ast.v);
  } else {
    return ast.location;
  }
}

template< template<typename T> class TPointer, typename TAst>
SourceLocation extract_location(TPointer<TAst> const & ast) {
  return extract_location(*ast);
}

}
