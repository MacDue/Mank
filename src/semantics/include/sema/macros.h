#pragma once

#include <vector>
#include <functional>

#include "ast/ast.h"

namespace Macros {

using ExprMacroExpander = std::function<Ast_Expression(Ast_Call&)>;

ExprMacroExpander const * get_expr_macro_expander(Ast_Macro_Identifier const & macro);

void register_macro(std::string name, ExprMacroExpander expander);

}
