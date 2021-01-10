#pragma once

#include <vector>
#include <functional>

#include "ast/ast.h"
#include "sema/type_infer.h"

struct AstBuilder;

namespace Macros {

#define MACRO_PARAMS Ast_Call&, AstBuilder&, Infer&, Lexer*
using ExprMacroExpander = std::function<Ast_Expression_Type(MACRO_PARAMS)>;

ExprMacroExpander const * get_expr_macro_expander(Ast_Macro_Identifier const & macro);

void register_macro(std::string name, ExprMacroExpander expander);

}
