#pragma once

#include "ast/ast.h"
#include "ast/util.h"
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
  throw_compile_error(AstHelper::extract_location(ast), format_pattern, args...);
  IMPOSSIBLE();
}
