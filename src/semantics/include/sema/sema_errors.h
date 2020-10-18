#pragma once

#include "ast/ast.h"
#include "errors/compiler_errors.h"
#include "errors/compiler_message.h"

/*
  RUN WAY!!!
  This file is just plain stupid template stuff to make life a little nicer.
  You don't need to read or understand these.

  It just works™
*/

#define IMPOSSIBLE() assert(false && "should be unreachable");

template< typename TAst, typename TPattern, typename... TArgs>
[[ noreturn ]] void throw_sema_error_at(
  TAst const & ast, TPattern format_pattern, TArgs const & ... args
) {
  FORWARD_MESSAGE(throw_compile_error);
  IMPOSSIBLE();
}

template< template<typename T> class TPointer, typename TAst, typename TPattern, typename... TArgs>
[[ noreturn ]] void throw_sema_error_at(
  TPointer<TAst> const & ast, TPattern format_pattern, TArgs const & ... args
) {
  throw_sema_error_at(*ast, format_pattern, args...);
}
