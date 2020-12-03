#pragma once

#include <vector>
#include "ast/self_helper.h"

DEF_TYPE(TupleType) {
  std::vector<Type_Ptr> element_types;
};
