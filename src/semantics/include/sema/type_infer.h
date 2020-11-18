#pragma once

#include <set>
#include <utility>

#include "ast/types.h"

namespace Infer {

using Constraint = std::pair<Type const *, Type const *>;

std::set<Constraint> collect_constraints(Ast_Function_Declaration& function);

}
