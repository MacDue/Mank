#pragma once

#include "ast/node.h"

struct FixedSizeArrayType {
  Type_Ptr element_type;
  uint size;
};
