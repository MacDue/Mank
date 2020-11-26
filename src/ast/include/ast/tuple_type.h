#pragma once

#include <vector>
#include "ast/defs.h"

struct TupleType {
  std::vector<Type_Ptr> element_types;
};
