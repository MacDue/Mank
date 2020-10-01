#pragma once

#include <cassert>
#include <stdexcept>
#include <formatxx/std_string.h>

#include "source_location.h"

struct CompilerError: public std::exception {
  SourceLocation location;
  std::string error_message;
public:
  CompilerError(SourceLocation location, std::string error_message)
    : location{location}, error_message{error_message} {}

  char const * what() const noexcept override {
    return error_message.c_str();
  }
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
