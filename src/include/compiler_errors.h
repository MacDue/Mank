#pragma once

#include <cassert>
#include <stdexcept>
#include <formatxx/std_string.h>

struct Lexer; // forward declare

#include "source_location.h"

class CompilerError: public std::exception {
  Lexer* source_lexer = nullptr; // can be set once the exception is caught.
  SourceLocation location;
  std::string error_message;
public:
  CompilerError(SourceLocation location, std::string error_message)
    : location{location}, error_message{error_message} {}

  void set_lexing_context(Lexer& lexer) {
    this->source_lexer = &lexer;
  }

  char const * what() const noexcept override {
    return error_message.c_str();
  }

  friend std::ostream& operator<< (std::ostream& stream, CompilerError const & error);
};

template<typename T>
[[ noreturn ]] void throw_compile_error(SourceLocation location, T error_message) {
  throw CompilerError(location, error_message);
}

template<typename TPattern, typename... TArgs>
[[ noreturn ]] void throw_compile_error(
  SourceLocation location, TPattern format_pattern, TArgs const & ... args
) {
  throw_compile_error(location,
    formatxx::format_string(format_pattern, args...));
}
