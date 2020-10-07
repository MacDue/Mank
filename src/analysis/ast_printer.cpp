#include <type_traits>
#include <mpark/patterns.hpp>

#include "types.h"
#include "ast_printer.h"
#include "token_helpers.h"

// Little hack that allows depth to be incremented/decremented
// when print functions are called/return (see DepthUpdate)
#define self (*this)

/* Constructs */

void AstPrinter::print_file(Ast_File& file) {
  putf("* File with {} functions", file.functions.size());
  if (!hide_lex_details) {
    putf("- Source name: {}", file.filename);
  }
  for (auto& func: file.functions) {
    self->print_function(std::get<Ast_Function_Declaration>(func->v));
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
    putf("- Return type: {}", type_to_string(*func.return_type));
  }
  if (func.arguments.size() > 0) {
    putf("- Arguments:");
    for (auto& arg: func.arguments) {
      indent(); putf(" {} : {}", arg.name.name, type_to_string(*arg.type));
    }
  }
  if (!func.external) {
    putf("- Body:");
    self->print_stmt(func.body);
  }
}

/* Statements */

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

/* Expressions */

void AstPrinter::print_expr(Ast_Expression& expr) {
  std::visit([&](auto& expr){
    if (!hide_lex_details) {
      putf("- Location: {}:{} -> {}:{}",
        expr.location.start_line, expr.location.start_column,
        expr.location.end_line, expr.location.end_column);
    }
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

void AstPrinter::print_expr(Ast_Literal& literal) {
  putf("* Literal");
  putf(
    literal.literal_type == PrimativeTypeTag::STRING
      ? "- {}: \"{}\"" : "- {}: {}",
      literal_type_to_string(literal.literal_type),
      literal.value);
}

void AstPrinter::print_expr(Ast_Identifier& ident) {
  putf("* Identifier");
  putf("- {}", ident.name);
}

static char const * operation_to_string(Ast_Operator operation) {
  return token_type_to_string(static_cast<TokenType>(operation));
}

void AstPrinter::print_const(PrimativeValue& const_value) {
  std::visit([&](auto value) {
    if constexpr (std::is_fundamental_v<decltype(value)>) {
      putf("- Const value: {}", value);
    } else {
      putf("- Not yet found to be const");
    }
  }, const_value);
}

void AstPrinter::print_expr(Ast_Unary_Operation& unary) {
  putf("* Unary operation");
  putf("- Operation: {}", operation_to_string(unary.operation));
  print_const(unary.const_expr_value);
  putf("- Operand:");
  self->print_expr(*unary.operand);
}

void AstPrinter::print_expr(Ast_Binary_Operation& binop) {
  putf("* Binary operation");
  putf("- Operation: {}", operation_to_string(binop.operation));
  print_const(binop.const_expr_value);
  putf("- Left:");
  self->print_expr(*binop.left);
  putf("- Right:");
  self->print_expr(*binop.right);
}

