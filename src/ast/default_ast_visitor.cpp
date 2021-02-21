#include <variant>

#include "ast/visitor.h"

#define recur (*this)

/* Constructs */

void BaseAstVisitor::operator()(Ast_File& file) {
  before(file);
  visit(file);
  for (auto func: file.functions) {
    recur(*func);
  }
  after(file);
}

void BaseAstVisitor::operator()(Ast_Function_Declaration& func) {
  before(func);
  visit(func);
  if (!func.external) {
    recur(func.body);
  }
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
  if (return_stmt.expression) {
    std::visit(recur, return_stmt.expression->v);
  }
  after(return_stmt);
}

void BaseAstVisitor::operator()(Ast_Assign& assign) {
  before(assign);
  visit(assign);
  std::visit(recur, assign.target->v);
  std::visit(recur, assign.expression->v);
  after(assign);
}

void BaseAstVisitor::operator()(Ast_Variable_Declaration& var_decl) {
  before(var_decl);
  visit(var_decl);
  if (var_decl.initializer) {
    std::visit(recur, var_decl.initializer->v);
  }
  after(var_decl);
}

void BaseAstVisitor::operator()(Ast_For_Loop& for_loop) {
  before(for_loop);
  visit(for_loop);
  std::visit(recur, for_loop.start_range->v);
  std::visit(recur, for_loop.end_range->v);
  recur(for_loop.body);
  after(for_loop);
}

void BaseAstVisitor::operator()(Ast_Structural_Binding& binding) {
  before(binding);
  visit(binding);
  std::visit(recur, binding.initializer->v);
  after(binding);
}

void BaseAstVisitor::operator()(Ast_Loop& loop) {
  before(loop);
  visit(loop);
  recur(loop.body);
  after(loop);
}

void BaseAstVisitor::operator()(Ast_While_Loop& while_loop) {
  before(while_loop);
  visit(while_loop);
  std::visit(recur, while_loop.cond->v);
  recur(while_loop.body);
  after(while_loop);
}

void BaseAstVisitor::operator()(Ast_Loop_Control& loop_control) {
  before(loop_control);
  visit(loop_control);
  after(loop_control);
}

void BaseAstVisitor::operator()(Ast_Constant_Declaration& const_decl) {
  before(const_decl);
  visit(const_decl);
  std::visit(recur, const_decl.const_expression->v);
  after(const_decl);
}

/* Expressions */

void BaseAstVisitor::operator()(Ast_Block& block) {
  before(block);
  visit(block);
  for (auto stmt: block.statements) {
    std::visit(recur, stmt->v);
  }
  after(block);
}

void BaseAstVisitor::operator()(Ast_If_Expr& if_expr) {
  before(if_expr);
  visit(if_expr);
  std::visit(recur, if_expr.cond->v);
  std::visit(recur, if_expr.then_block->v);
  if (if_expr.has_else) {
    std::visit(recur, if_expr.else_block->v);
  }
  after(if_expr);
}

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

void BaseAstVisitor::operator()(Ast_Field_Access& access) {
  before(access);
  visit(access);
  std::visit(recur, access.object->v);
  after(access);
}

void BaseAstVisitor::operator()(Ast_Array_Literal& array) {
  before(array);
  visit(array);
  for (auto& element: array.elements) {
    std::visit(recur, element->v);
  }
  after(array);
}

void BaseAstVisitor::operator()(Ast_Index_Access& index) {
  before(index);
  visit(index);
  std::visit(recur, index.object->v);
  std::visit(recur, index.index->v);
  after(index);
}

void BaseAstVisitor::operator()(Ast_Lambda& lambda) {
  before(lambda);
  visit(lambda);
  recur(lambda.body);
}

void BaseAstVisitor::operator()(Ast_Macro_Identifier& macro_ident) {
  before(macro_ident);
  visit(macro_ident);
  after(macro_ident);
};

void BaseAstVisitor::operator()(Ast_Tuple_Literal& tuple) {
  before(tuple);
  visit(tuple);
  for (auto& element: tuple.elements) {
    std::visit(recur, element->v);
  }
  after(tuple);
}

void BaseAstVisitor::operator()(Ast_Pod_Literal& pod) {
  before(pod);
  visit(pod);
  for (auto& field: pod.fields) {
    std::visit(recur, field.initializer->v);
  }
  after(pod);
}

void BaseAstVisitor::operator()(Ast_As_Cast& as_cast) {
  before(as_cast);
  visit(as_cast);
  std::visit(recur, as_cast.object->v);
  after(as_cast);
}

void BaseAstVisitor::operator()(Ast_Array_Repeat& array_repeat) {
  before(array_repeat);
  visit(array_repeat);
  std::visit(recur, array_repeat.initializer->v);
  std::visit(recur, array_repeat.repetitions->v);
  after(array_repeat);
}

void BaseAstVisitor::operator()(Ast_Spawn& spawn) {
  before(spawn);
  visit(spawn);
  std::visit(recur, spawn.initializer->v);
  after(spawn);
}

void BaseAstVisitor::operator()(Ast_Specialized_Identifier& special_ident) {
  before(special_ident);
  visit(special_ident);
  after(special_ident);
}

void BaseAstVisitor::operator()(Ast_Path& path) {
  before(path);
  visit(path);
  after(path);
}

void BaseAstVisitor::operator()(Ast_Switch_Expr& switch_expr) {
  before(switch_expr);
  visit(switch_expr);
  std::visit(recur, switch_expr.switched->v);
  for (auto& switch_case: switch_expr.cases) {
    std::visit(recur, switch_case.match->v);
    recur(switch_case.body);
  }
  after(switch_expr);
}
