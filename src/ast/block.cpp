#include <string>
#include "ast/stmt.h"
#include "ast/block.h"

Expr_Ptr Ast_Block::get_final_expr() const {
  if (has_final_expr) {
    auto& expr_stmt = std::get<Ast_Expression_Statement>(statements.back()->v);
    return expr_stmt.expression;
  }
  return nullptr;
}
