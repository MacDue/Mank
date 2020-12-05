#pragma once

#include "ast/expr.h"

namespace AstHelper {

void rewrite_expr(Ast_Expression& expr, Ast_Expression_Type rewrite);

}
