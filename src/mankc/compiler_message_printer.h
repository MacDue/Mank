#pragma once

#include <ostream>

#include "compiler_errors.h"
#include "compiler_message.h"

std::ostream& operator<< (std::ostream& os, CompilerMessage const & message);

inline std::ostream& operator<< (std::ostream& os, CompilerError const & error) {
  return os << error.error_message;
}
