#pragma once

#include <set>
#include <map>
#include <utility>

#include "ast/types.h"

namespace Infer {

using Constraint = std::pair<Type_Ptr, Type_Ptr>;
using ConstraintSet = std::set<Constraint>;
using Substitution = std::map<uint32_t, Type_Ptr>;

Substitution unify(ConstraintSet && constraints);

}
