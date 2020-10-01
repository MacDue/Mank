#pragma once

#include <cassert>
#include <type_traits>

#define IMPOSSIBLE() assert(false && "should be unreachable");

#include "ast.h"
#include "is_abstract_ast.h"
#include "compiler_errors.h"

/*
  RUN WAY!!!
  This file is just plain stupid template stuff to make life a little nicer.
  You don't need to read or understand these.

  It just works™
*/

template<typename T>
[[ noreturn ]] void throw_sema_error_at(Ast_Identifier const & ident, T format_pattern) {
  throw_compile_error(ident.location, format_pattern, ident.name);
}

template< template<typename T> class TPointer, typename TAst, typename TPattern, typename... TArgs>
[[ noreturn ]] void throw_sema_error_at(
  TPointer<TAst> const & ast, TPattern format_pattern, TArgs const & ... args
) {
  throw_sema_error_at(*ast, format_pattern, args...);
}

template< typename TAst, typename TPattern, typename... TArgs>
[[ noreturn ]] void throw_sema_error_at(
  TAst const & ast, TPattern format_pattern, TArgs const & ... args
) {
  if constexpr (is_abstract_ast<TAst>::value) {
    std::visit([&](auto const & ast) {
      throw_compile_error(ast.location, format_pattern, args...);
    }, ast.v);
  } else {
    throw_compile_error(ast.location, format_pattern, args...);
  }
  IMPOSSIBLE();
}
