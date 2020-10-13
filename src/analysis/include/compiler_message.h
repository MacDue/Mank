#pragma once

#include <string>
#include <cassert>
#include <ostream>

#include "source_location.h"
#include "ast/is_abstract_ast.h"

/*
  Used when formatting errors/warnings.
  It's used a few places (there's no great reason for it to live in this file)
*/
#define FORWARD_MESSAGE(message_formatter)                       \
  if constexpr (is_abstract_ast<TAst>::value) {                  \
    std::visit([&](auto const & ast) {                           \
      message_formatter(ast.location, format_pattern, args...);  \
    }, ast.v);                                                   \
  } else {                                                       \
    message_formatter(ast.location, format_pattern, args...);    \
  }

struct Lexer; // forward declare

struct CompilerMessage {
  SourceLocation location;
  std::string message;
  enum Type {
    ERROR,
    WARNING
  } type = ERROR;
  mutable Lexer* source_lexer = nullptr; // can be set once the exception is caught.
};

std::ostream& operator<< (std::ostream& os, CompilerMessage const & message);
