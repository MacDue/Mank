#pragma once

#include "ast/ast.h"
#include "ast/visitor.h"

namespace AstHelper {

/* This is still very limited */
struct ConstantVisitor: BaseAstVisitor {
  void after(Ast_Binary_Operation& binop) override;
  void after(Ast_Unary_Operation& unary) override;
  void visit(Ast_Literal& literal) override;
};

template <typename TAst>
void constant_propagate(TAst& ast) {
  ConstantVisitor const_visitor;
  const_visitor(ast);
}

}
