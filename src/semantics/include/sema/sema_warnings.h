#pragma once

#include "ast/util.h"

/*
  Nasty stack of templates for emitting formattted warnings.
  Not very important.

  I don't really want this within the headers, but it's stuck here for now,
  as these have to be member functions of Semantics.
*/

template <typename TPattern, typename... TArgs>
void append_warning(SourceLocation location, TPattern format_pattern, TArgs const ... args) {
  auto warning_message = formatxx::format_string(format_pattern, args...);
  warnings.push_back(CompilerMessage{location, warning_message, CompilerMessage::WARNING});
}

template <typename TAst, typename TPattern, typename... TArgs>
void emit_warning_at(TAst const & ast, TPattern format_pattern, TArgs const & ... args) {
  append_warning(AstHelper::extract_location(ast), format_pattern, args...);
}
