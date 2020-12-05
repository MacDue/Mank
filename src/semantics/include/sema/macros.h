#pragma once

#include <vector>
#include <functional>

#include "ast/ast.h"
#include "sema/type_infer.h"

namespace Macros {

using ExprMacroExpander = std::function<Ast_Expression_Type(Ast_Call&, Infer&)>;

ExprMacroExpander const * get_expr_macro_expander(Ast_Macro_Identifier const & macro);

void register_macro(std::string name, ExprMacroExpander expander);

}
