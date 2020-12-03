#pragma once

#include "ast/node.h"

DEF_TYPE(FixedSizeArrayType) {
  Type_Ptr element_type;
  uint size;
};
