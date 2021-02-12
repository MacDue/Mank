#pragma once

#include <cassert>
#include <ostream>
#include <stdexcept>
#include <formatxx/std_string.h>

#include "compiler_message.h"

class CompilerError: public std::exception {
  CompilerMessage error_message;
public:
  CompilerError(CompilerMessage error_message)
    : error_message{error_message} {}

  void set_lexing_context(Lexer& lexer) {
    this->error_message.source_lexer = &lexer;
  }

  char const * what() const noexcept override {
    return error_message.message.c_str();
  }

  friend std::ostream& operator<< (std::ostream& stream, CompilerError const & error);
};

template<typename TPattern, typename... TArgs>
[[ noreturn ]] void throw_compile_error(
  std::optional<SourceLocation> location, TPattern format_pattern, TArgs const & ... args
) {
  auto error_message = formatxx::format_string(format_pattern, args...);
  throw CompilerError(CompilerMessage{location, error_message});;
}

template<typename TPattern, typename... TArgs>
[[ noreturn ]] void throw_general_error(TPattern format_pattern, TArgs const & ... args) {
  throw_compile_error(std::nullopt, format_pattern, args...);
}
