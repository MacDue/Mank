#pragma once

#include "node.h"

struct FixedSizeArrayType {
  Type_Ptr element_type;
  uint size;
};
