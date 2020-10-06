#include <variant>

#include "ast/visitor.h"

#define recur (*this)

/* Constructs */

void AstVisitor::operator()(Ast_File& file) {
  for (auto& func_type: file.functions) {
    auto& func = std::get<Ast_Function_Declaration>(func_type->v);
    recur(func);
  }
}

void AstVisitor::operator()(Ast_Function_Declaration& func) {
  recur(func.body);
}

/* Statements */

void AstVisitor::operator()(Ast_Expression_Statement& expr_stmt) {
  std::visit(recur, expr_stmt.expression->v);
}

void AstVisitor::operator()(Ast_Return_Statement& return_stmt) {
  std::visit(recur, return_stmt.expression->v);
}

void AstVisitor::operator()(Ast_Block& block) {
  for (auto& stmt: block.statements) {
    std::visit(recur, stmt->v);
  }
}

void AstVisitor::operator()(Ast_If_Statement& if_stmt) {
  std::visit(recur, if_stmt.cond->v);
  std::visit(recur, if_stmt.then_block->v);
  if (if_stmt.has_else) {
    std::visit(recur, if_stmt.else_block->v);
  }
}

/* Expressions */

void AstVisitor::operator()(Ast_Call& call) {
  std::visit(recur, call.callee->v);
  for (auto& arg: call.arguments) {
    std::visit(recur, arg->v);
  }
}

void AstVisitor::operator()(Ast_Literal& literal) {
  /* that'll do pig, that'll do */
  (void) literal;
}

void AstVisitor::operator()(Ast_Identifier& ident) {
  /* we've hit rock bottom */
  (void) ident;
}

void AstVisitor::operator()(Ast_Unary_Operation& unary) {
  std::visit(recur, unary.operand->v);
}

void AstVisitor::operator()(Ast_Binary_Operation& binop) {
  std::visit(recur, binop.left->v);
  std::visit(recur, binop.right->v);
}
