#pragma once

#include <cassert>

#include "source_location.h"
/* stub */


template<typename ErrorMsg>
[[ noreturn ]] void throw_compile_error(SourceLocation location, ErrorMsg error_message) {
  assert(false && "not implemented");
}
