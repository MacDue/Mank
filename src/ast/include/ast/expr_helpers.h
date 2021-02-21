#pragma once

#include <utility>

#include "ast/expr.h"
#include "ast/enum_type.h"

struct Scope;

namespace AstHelper {

void rewrite_expr(Ast_Expression& expr, Ast_Expression_Type rewrite);

std::pair<EnumType*, EnumType::Member*> path_as_enum_member(Ast_Path& path, Scope& scope);

}
