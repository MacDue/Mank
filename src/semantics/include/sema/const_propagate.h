#pragma once

#include "ast/ast.h"
#include "ast/visitor.h"

namespace AstHelper {

/* This is still very limited */
struct ConstantVisitor: BaseAstVisitor {
  void after(Ast_Binary_Operation& binop) override;
  void after(Ast_Unary_Operation& unary) override;
  void after(Ast_Index_Access& index) override;
  void visit(Ast_Literal& literal) override;
};

template <typename TAst>
void constant_propagate(TAst& ast) {
  ConstantVisitor const_visitor;
  const_visitor(ast);
}

inline void constant_expr_eval(Ast_Expression& expr) {
  ConstantVisitor const_visitor;
  std::visit(const_visitor, expr.v);
}

}
