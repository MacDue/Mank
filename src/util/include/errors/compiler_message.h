#pragma once

#include <string>
#include <cassert>

#include "ast/source_location.h"
#include "ast/is_abstract_ast.h"

struct Lexer; // forward declare

struct CompilerMessage {
  SourceLocation location;
  std::string message;
  enum Type {
    ERROR,
    WARNING,
    NOTE
  } type = ERROR;
  mutable Lexer* source_lexer = nullptr; // can added later for printing with context
};
