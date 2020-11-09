#pragma once

#include <vector>

#include "node.h"

struct LambdaType {
  std::vector<Type_Ptr> argument_types;
  Type_Ptr return_type;
};
