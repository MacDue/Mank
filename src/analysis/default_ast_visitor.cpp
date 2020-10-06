#include <variant>

#include "ast/visitor.h"

#define recur (*this)

/* Constructs */

void BaseAstVisitor::operator()(Ast_File& file) {
  before(file);
  visit(file);
  for (auto& func_type: file.functions) {
    auto& func = std::get<Ast_Function_Declaration>(func_type->v);
    recur(func);
  }
  after(file);
}

void BaseAstVisitor::operator()(Ast_Function_Declaration& func) {
  before(func);
  visit(func);
  recur(func.body);
  after(func);
}

/* Statements */

void BaseAstVisitor::operator()(Ast_Expression_Statement& expr_stmt) {
  before(expr_stmt);
  visit(expr_stmt);
  std::visit(recur, expr_stmt.expression->v);
  after(expr_stmt);
}

void BaseAstVisitor::operator()(Ast_Return_Statement& return_stmt) {
  before(return_stmt);
  visit(return_stmt);
  std::visit(recur, return_stmt.expression->v);
}

void BaseAstVisitor::operator()(Ast_Block& block) {
  before(block);
  visit(block);
  for (auto& stmt: block.statements) {
    std::visit(recur, stmt->v);
  }
  after(block);
}

void BaseAstVisitor::operator()(Ast_If_Statement& if_stmt) {
  before(if_stmt);
  visit(if_stmt);
  std::visit(recur, if_stmt.cond->v);
  std::visit(recur, if_stmt.then_block->v);
  if (if_stmt.has_else) {
    std::visit(recur, if_stmt.else_block->v);
  }
  after(if_stmt);
}

/* Expressions */

void BaseAstVisitor::operator()(Ast_Call& call) {
  before(call);
  visit(call);
  std::visit(recur, call.callee->v);
  for (auto& arg: call.arguments) {
    std::visit(recur, arg->v);
  }
  after(call);
}

void BaseAstVisitor::operator()(Ast_Literal& literal) {
  before(literal);
  visit(literal);
  after(literal);
}

void BaseAstVisitor::operator()(Ast_Identifier& ident) {
  before(ident);
  visit(ident);
  after(ident);
}

void BaseAstVisitor::operator()(Ast_Unary_Operation& unary) {
  before(unary);
  visit(unary);
  std::visit(recur, unary.operand->v);
  after(unary);
}

void BaseAstVisitor::operator()(Ast_Binary_Operation& binop) {
  before(binop);
  visit(binop);
  std::visit(recur, binop.left->v);
  std::visit(recur, binop.right->v);
  after(binop);
}
