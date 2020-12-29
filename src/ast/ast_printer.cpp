#include <type_traits>
#include <mpark/patterns.hpp>

#include "ast/types.h"
#include "ast/ast_printer.h"
#include "parser/token_helpers.h"

// Little hack that allows depth to be incremented/decremented
// when print functions are called/return (see DepthUpdate)
#define self (*this)

/* Constructs */

void AstPrinter::print_file(Ast_File& file) {
  putf("* File with {} pods, and {} functions",
    file.pods.size(), file.functions.size());
  if (!hide_lex_details) {
    putf("- Source name: {}", file.filename);
  }
  for (auto pod: file.pods) {
    self->print_pod(*pod);
    putf("");
  }
  for (auto func: file.functions) {
    self->print_function(*func);
    putf("");
  }
}

void AstPrinter::print_args(std::vector<Ast_Argument> args) {
  for (auto& arg: args) {
    indent(); putf(" {} : {}", arg.name.name, type_to_string(arg.type));
  }
}

void AstPrinter::print_pod(Ast_Pod_Declaration& pod) {
  putf("* Pod {}", pod.identifier.name);
  if (pod.fields.size() > 0) {
    putf("- Fields:");
    self->print_args(pod.fields);
  } else {
    putf("- No fields");
  }
}

void AstPrinter::print_function(Ast_Function_Declaration& func) {
  putf("* {} {}",
    func.procedure ? "Procedure" : "Function",
    func.identifier.name);
  putf("- External: {}", func.external);
  putf("- C function: {}", func.external);
  if (!func.procedure) {
    putf("- Return type: {}", type_to_string(func.return_type));
  }
  if (func.arguments.size() > 0) {
    putf("- Arguments:");
    self->print_args(func.arguments);
  }
  if (!func.external) {
    putf("- Body:");
    self->print_expr(func.body);
  }
}

/* Statements */

void AstPrinter::print_stmt(Ast_Statement& stmt) {
  std::visit([&](auto& stmt) {
    self->print_stmt(stmt);
  }, stmt.v);
}

void AstPrinter::print_stmt(Ast_Expression_Statement& expr_stmt) {
  putf("* Expression statement");
  putf("- Value:");
  self->print_expr(*expr_stmt.expression);
}

void AstPrinter::print_stmt(Ast_Return_Statement& return_stmt) {
  putf("* Return statement");
  if (return_stmt.expression) {
    putf("- Value:");
    self->print_expr(*return_stmt.expression);
  }
}

void AstPrinter::print_stmt(Ast_Assign& assign) {
  putf("* Assign statement");
  putf("- Target:");
  self->print_expr(*assign.target);
  putf("- Value:");
  self->print_expr(*assign.expression);
}

void AstPrinter::print_stmt(Ast_Variable_Declaration& var_decl) {
  putf("* Variable declaration");
  putf("- {} : {}", var_decl.variable.name, type_to_string(var_decl.type));
  if (var_decl.initializer) {
    putf("- Initializer:");
    self->print_expr(*var_decl.initializer);
  }
}

void AstPrinter::print_stmt(Ast_For_Loop& for_loop) {
  putf("* For loop");
  putf("- Loop variable: {} : {}", for_loop.loop_variable.name,
    type_to_string(for_loop.type));
  putf("- Range start:");
  self->print_expr(*for_loop.start_range);
  putf("- Range end:");
  self->print_expr(*for_loop.end_range);
  putf("- Loop body:");
  self->print_expr(for_loop.body);
}

void AstPrinter::print_binding(TupleBinding& binding) {
  using namespace mpark::patterns;
  for (auto& binding: binding.binds) {
    match(binding)(
      pattern(as<Ast_Argument>(arg)) = [&](auto& arg) {
        self->print_args({ arg });
      },
      pattern(as<TupleBinding>(arg)) = [&](auto& nested) {
        // FIXME: Hack
        indent(); indent(); self->putf("- Nested bindings");
        self->print_binding(nested);
      }
    );
  }
}

void AstPrinter::print_stmt(Ast_Tuple_Structural_Binding& tuple_binding) {
  putf("* Tuple structural binding");
  putf("- Bindings");
  this->print_binding(tuple_binding.bindings);
  putf("- Initializer");
  self->print_expr(*tuple_binding.initializer);
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

void AstPrinter::print_expr(Ast_Block& block) {
  putf("* Block with {} statements", block.statements.size());
  if (block.has_final_expr) {
    putf("- Block terminates with expression");
  }
  for (auto stmt: block.statements) {
    self->print_stmt(*stmt);
  }
}

void AstPrinter::print_expr(Ast_If_Expr& if_stmt) {
  putf("* If expr{}", if_stmt.else_block ? "" : " with no else");
  putf("- Condition:");
  self->print_expr(*if_stmt.cond);
  putf("- Then:");
  self->print_expr(*if_stmt.then_block);
  if (if_stmt.has_else) {
    putf("- Else:");
    self->print_expr(*if_stmt.else_block);
  }
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
  putf("- {}: {}",
    PrimativeType::type_name(literal.literal_type),
    literal.value);
}

void AstPrinter::print_expr(Ast_Identifier& ident) {
  putf("* Identifier");
  putf("- {}", ident.name);
}

void AstPrinter::print_expr(Ast_Macro_Identifier& macro_ident) {
  putf("* Macro identifier");
  putf("- {}", macro_ident.name);
}

static char const * operation_to_string(Ast_Operator operation) {
  return token_type_to_string(static_cast<TokenType>(operation));
}

void AstPrinter::print_const(PrimativeValue const_value) {
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
  print_const(unary.const_value());
  putf("- Operand:");
  self->print_expr(*unary.operand);
}

void AstPrinter::print_expr(Ast_Binary_Operation& binop) {
  putf("* Binary operation");
  putf("- Operation: {}", operation_to_string(binop.operation));
  print_const(binop.const_value());
  putf("- Left:");
  self->print_expr(*binop.left);
  putf("- Right:");
  self->print_expr(*binop.right);
}

void AstPrinter::print_expr(Ast_Field_Access& access) {
  putf("* Field access");
  putf("- Field: {}", access.field.name);
  putf("- Object:");
  self->print_expr(*access.object);
}

void AstPrinter::print_expr(Ast_Array_Literal& array) {
  putf("* Array literal");
  putf("- {} elements", array.elements.size());
  uint element_idx = 0;
  for (auto element: array.elements) {
    indent(); putf("[{}]:", element_idx);
    self->print_expr(*element);
    ++element_idx;
  }
}

void AstPrinter::print_expr(Ast_Index_Access& index) {
  putf("* Index access");
  putf("- Index:");
  self->print_expr(*index.index);
  putf("- Object:");
  self->print_expr(*index.object);
}

void AstPrinter::print_expr(Ast_Lambda& lambda) {
  putf("* Lambda");
  putf("- Return type: {}", type_to_string(lambda.return_type));
  if (lambda.arguments.size() > 0) {
    putf("- Arguments:");
    self->print_args(lambda.arguments);
  }
  putf("- Body:");
  self->print_expr(lambda.body);
}

void AstPrinter::print_expr(Ast_Tuple_Literal& tuple) {
  putf("* Tuple literal");
  putf("- {} elements", tuple.elements.size());
  uint element_idx = 0;
  for (auto element: tuple.elements) {
    indent(); putf(".{}:", element_idx);
    self->print_expr(*element);
    ++element_idx;
  }
}

void AstPrinter::print_expr(Ast_Pod_Literal& pod) {
  putf("* Pod literal");
  putf("- Pod: {}", type_to_string(pod.pod));
  for (auto field: pod.fields) {
    indent();
    putf(".{}:", field.field.name);
    self->print_expr(*field.initializer);
  }
}
