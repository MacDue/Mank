#include <string>
#include <unordered_map>

#include "sema/macros.h"

namespace Macros {

static std::unordered_map<std::string, ExprMacroExpander> expression_expanders;

ExprMacroExpander const * get_expr_macro_expander(Ast_Macro_Ident const & macro) {
  if (expression_expanders.contains(macro.name)) {
    return &expression_expanders.at(macro.name);
  }
  return nullptr;
}

void register_macro(std::string name, ExprMacroExpander expander) {
  expression_expanders[name] = expander;
}

}
