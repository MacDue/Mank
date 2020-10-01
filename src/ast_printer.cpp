#include "ast_printer.h"

// Little hack that allows depth to be incremented/decremented
// when print functions are called/return (see DepthUpdate)
#define self (*this)

void AstPrinter::print_file(Ast_File& file) {
  putf("* File with {} functions", file.functions.size());
  putf("- Source name: {}", file.filename);
  for (auto& func: file.functions) {
    self->print_function(*func);
    putf("");
  }
}

void AstPrinter::print_function(Ast_Function_Declaration& func) {
  putf("* {} {}",
    func.procedure ? "Procedure" : "Function",
    func.identifer.name);
  putf("- External: {}", func.external);
  putf("- C function: {}", func.external);
  if (!func.procedure) {
    putf("- Return type: TODO");
  }
  if (func.arguments.size() > 0) {
    putf("- Arguments:");
    for (auto& arg: func.arguments) {
      indent(); putf(" TODO : TODO");
    }
  }
  if (!func.external) {
    putf("- Body:");
    self->print_stmt(func.body);
  }
}

void AstPrinter::print_stmt(Ast_Statement& stmt) {
  std::visit([&](auto& stmt) {
    self->print_stmt(stmt);
  }, stmt.v);
}

void AstPrinter::print_stmt(Ast_Block& block) {
  putf("* Block with {} statements", block.statements.size());
  for (auto& stmt: block.statements) {
    self->print_stmt(*stmt);
  }
}

void AstPrinter::print_stmt(Ast_If_Statement& if_stmt) {
  putf("* If statement{}", if_stmt.else_block ? "" : " with no else");
  putf("- Condition:");
  self->print_expr(*if_stmt.cond);
  putf("- Then:");
  self->print_stmt(*if_stmt.then_block);
  if (if_stmt.has_else) {
    putf("- Else:");
    self->print_stmt(*if_stmt.else_block);
  }
}

void AstPrinter::print_stmt(Ast_Expression_Statement& expr_stmt) {
  putf("* Expression statement");
  putf("- Value");
  self->print_expr(*expr_stmt.expression);
}

void AstPrinter::print_stmt(Ast_Return_Statement& return_stmt) {
  putf("* Return statement");
  putf("- Value");
  self->print_expr(*return_stmt.expression);
}

void AstPrinter::print_expr(Ast_Expression& expr) {
  std::visit([&](auto& expr){
    self->print_expr(expr);
  }, expr.v);
}

void AstPrinter::print_expr(Ast_Call& call) {
  putf("* Call");
  putf("- Callee:");
  self->print_expr(*call.callee);
  if (call.arguments.size() > 0) {
    putf("- Arguements");
    uint arg_idx = 0;
    for (auto& arg: call.arguments) {
      indent(); putf("{:03}:", arg_idx);
      self->print_expr(*arg);
      ++arg_idx;
    }
  }
}

static char const * literal_type_to_string(LiteralType type) {
  switch (type) {
    case LiteralType::FLOAT32:
      return "Float32";
    case LiteralType::FLOAT64:
      return "Float64";
    case LiteralType::INTEGER:
      return "Integer";
    case LiteralType::STRING:
      return "Static string";
    default:
      return "???";
  }
}

void AstPrinter::print_expr(Ast_Literal& literal) {
  putf("* Literal");
  putf(
    literal.literal_type == LiteralType::STRING
      ? "- {}: \"{}\"" : "- {}: {}",
      literal_type_to_string(literal.literal_type),
      literal.value);
}

void AstPrinter::print_expr(Ast_Identifier& ident) {
  putf("* Identifier");
  putf("- {}", ident.name);
}

char const * operation_to_string(Ast_Operator op) {
  /* No great way in C++ */
  switch (op)
  {
    case Ast_Operator::BITWISE_NOT: return "~ (BITWISE_NOT)";
    case Ast_Operator::BITWISE_AND: return "& (BITWISE_AND)";
    case Ast_Operator::BITWISE_OR: return "| (BITWISE_OR)";

    case Ast_Operator::PLUS: return "+ (ADD)";
    case Ast_Operator::MINUS: return "- (MINUS)";
    case Ast_Operator::DIVIDE: return "/ (DIVIDE)";
    case Ast_Operator::TIMES: return "* (TIMES)";
    case Ast_Operator::MODULO: return "% (MODULO)";

    case Ast_Operator::LESS_THAN: return "< (LESS_THAN)";
    case Ast_Operator::GREATER_THAN: return "> (GREATER_THAN)";

    case Ast_Operator::BITWISE_XOR: return "|! (BITWISE_XOR)";

    case Ast_Operator::LOGICAL_OR: return "|| (LOGICAL_OR)";
    case Ast_Operator::LOGICAL_NOT: return "¬ (LOGICAL_NOT)";
    case Ast_Operator::LEFT_SHIFT: return "<< (LEFT_SHIFT)";
    case Ast_Operator::RIGHT_SHIFT: return ">> (RIGHT_SHIFT)";
    case Ast_Operator::GREATER_EQUAL: return ">= (GREATER_EQUAL)";
    case Ast_Operator::LESS_EQUAL: return "<= (LESS_EQUAL)";
    case Ast_Operator::EQUAL_TO: return "== (EQUAL_TO)";
    case Ast_Operator::NOT_EQUAL_TO: return "!= (NOT_EQUAL_TO)";

    case Ast_Operator::LOGICAL_AND: return "&& (LOGICAL_AND)";
    default: return "?? (UNKNOWN)";
  }
}

void AstPrinter::print_expr(Ast_Unary_Operation& unary) {
  putf("* Unary operation");
  putf("- Operation: {}", operation_to_string(unary.operation));
  putf("- Operand:");
  self->print_expr(*unary.operand);
}

void AstPrinter::print_expr(Ast_Binary_Operation& binop) {
  putf("* Binary operation");
  putf("- Operation: {}", operation_to_string(binop.operation));
  putf("- Left:");
  self->print_expr(*binop.left);
  putf("- Right:");
  self->print_expr(*binop.right);
}

