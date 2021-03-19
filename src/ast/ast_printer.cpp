#include <type_traits>
#include <mpark/patterns.hpp>

#include "ast/types.h"
#include "ast/ast_printer.h"
#include "parser/token_helpers.h"

// Little hack that allows depth to be incremented/decremented
// when print functions are called/return (see DepthUpdate)
#define self (*this)
 // hack for nest blocks
#define nest DepthUpdate _du(this);

/* Constructs */

#define FOR_DECL_PRINT(decl, decls, print) {  \
  for (auto decl: decls) {                    \
    print;                                    \
    putf("");                                 \
  }                                           \
}

void AstPrinter::print_file(Ast_File& file) {
  using namespace mpark::patterns;
  // putf("* File with {} global consts, {} pods, and {} functions",
  //   file.global_consts.size(), file.pods.size(), file.functions.size());
  if (!hide_lex_details) {
    putf("- Source name: {}", file.filename);
  }

  FOR_DECL_PRINT(global_const, file.global_consts, ({
    self->print_stmt(*global_const);
  }))

  for (auto item: file.items) {
    match(item->v)(
      pattern(as<Ast_Pod_Declaration>(arg)) = [&](auto& pod_decl){
        self->print_pod(pod_decl);
      },
      pattern(as<Ast_Enum_Declaration>(arg)) = [&](auto& enum_decl){
        self->print_enum(enum_decl);
      },
      pattern(as<Ast_Type_Alias>(arg)) = [&](auto& type_alias){
        self->print_type_alias(type_alias);
      }
    );
    putf("");
  }

  FOR_DECL_PRINT(func, file.functions, ({
    self->print_function(*func);
  }))
}

void AstPrinter::print_args(std::vector<Ast_Argument> const & args) {
  for (auto& arg: args) {
    indent(); putf(" {} : {}", arg.name.name, type_to_string(arg.type));
  }
}

void AstPrinter::print_types(std::vector<Type_Ptr> const & types) {
  for (auto type: types) {
    indent();
    putf("{}", type_to_string(type));
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

void AstPrinter::print_type_alias(Ast_Type_Alias& type_alias) {
  putf("* Type alias");
  putf("- {} = {}", type_alias.alias.name, type_to_string(type_alias.type));
}


void AstPrinter::print_enum_members(
  std::vector<Ast_Enum_Declaration::Member> const & enum_members
) {
  using namespace mpark::patterns;
  for (auto& member: enum_members) {
    indent(); putf(" {}", member.tag.name);
    // Nasty nest blocks to fix formatting :(
    nest {
      nest {
        match(member.data)(
          pattern(some(as<Ast_Enum_Declaration::Member::TupleData>(arg))) = [&](auto& tuple_data) {
            putf("- Tuple data:");
            nest { self->print_types(tuple_data.elements); }
          },
          pattern(some(as<Ast_Enum_Declaration::Member::PodData>(arg))) = [&](auto& pod_data) {
            putf("- Pod data:");
            self->print_args(pod_data.fields);
          },
          pattern(_) = []{}
        );
      }
    }
  }
}

void AstPrinter::print_enum(Ast_Enum_Declaration& enum_decl) {
  putf("* Enum {}", enum_decl.identifier.name);
  if (enum_decl.members.size() > 0) {
    putf("- Members:");
    self->print_enum_members(enum_decl.members);
  } else {
    putf("- No members");
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

#define BINDS_PATTERN anyof(as<Ast_Tuple_Binds>(arg), as<Ast_Pod_Binds>(arg))

void AstPrinter::print_binding(Ast_Tuple_Binds const & tuple_binds) {
  using namespace mpark::patterns;
  putf("* Tuple bindings");
  for (auto& binding: tuple_binds.binds) {
    match(binding)(
      pattern(as<Ast_Bind>(arg)) = [&](auto& arg) {
        self->print_args({ arg });
      },
      pattern(BINDS_PATTERN) = [&](auto& nested) {
        self->print_binding(nested);
      }
    );
  }
}

void AstPrinter::print_binding(Ast_Pod_Binds const & pod_binds) {
  using namespace mpark::patterns;
  putf("* Pod bindings");
  for (auto& binding: pod_binds.binds) {
    DepthUpdate _(this); DepthUpdate __(this);DepthUpdate ___(this);// hack
    putf(".{}", binding.field.name);
    match(binding.replacement)(
      pattern(as<Ast_Bind>(arg)) = [&](auto& bind) {
        if (!bind.name.empty()) {
          indent(); putf("- Bound name: {}", bind.name.name);
        }
        indent(); putf("- Type: {}", type_to_string(bind.type));
      },
      pattern(BINDS_PATTERN) = [&](auto& nested) {
        self->print_binding(nested);
      }
    );
  }
}

void AstPrinter::print_binding(Ast_Binding const & binding) {
  std::visit([&](auto& bindings){
    this->print_binding(bindings);
  }, binding);
}

void AstPrinter::print_stmt(Ast_Structural_Binding& tuple_binding) {
  putf("* Structural binding");
  putf("- Bindings:");
  self->print_binding(tuple_binding.bindings);
  putf("- Initializer:");
  self->print_expr(*tuple_binding.initializer);
}

void AstPrinter::print_stmt(Ast_Loop& loop) {
  putf("* Loop");
  putf("- Body:");
  self->print_expr(loop.body);
}

void AstPrinter::print_stmt(Ast_While_Loop& while_loop) {
  putf("* While loop");
  putf("- Condition:");
  self->print_expr(*while_loop.cond);
  putf("- Body:");
  self->print_expr(while_loop.body);
}

void AstPrinter::print_stmt(Ast_Loop_Control& loop_control) {
  putf("* Loop control");
  switch (loop_control.type) {
    case Ast_Loop_Control::BREAK:
      putf("- Break");
      break;
    case Ast_Loop_Control::CONTINUE:
      putf("- Continue");
      break;
    default:
      assert(false && "fix me! unknown loop control");
      break;
  }
}

void AstPrinter::print_stmt(Ast_Constant_Declaration& const_decl) {
  putf("* Constant declaration");
  putf("- {} : {}", const_decl.constant.name, type_to_string(const_decl.type));
  putf("- Initializer:");
  self->print_expr(*const_decl.const_expression);
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
  putf("- Pod:");
  print_expr(pod.pod);
  if (pod.specializations.size() > 0) {
    putf("- Speilizations:");
    self->print_types(pod.specializations);
  }
  for (auto field: pod.fields) {
    indent();
    putf(".{}:", field.field.name);
    self->print_expr(*field.initializer);
  }
}

void AstPrinter::print_expr(Ast_As_Cast& as_cast) {
  putf("* As cast");
  putf("- Target type: {}", type_to_string(as_cast.type));
  putf("- Object:");
  self->print_expr(*as_cast.object);
}

void AstPrinter::print_expr(Ast_Array_Repeat& array_repeat) {
  putf("* Repeat array literal");
  putf("- Initializer:");
  self->print_expr(*array_repeat.initializer);
  putf("- Repetitions:");
  self->print_expr(*array_repeat.repetitions);
}

void AstPrinter::print_expr(Ast_Spawn& spawn) {
  putf("* Spawn");
  putf("- Initializer:");
  self->print_expr(*spawn.initializer);
}

void AstPrinter::print_expr(Ast_Specialized_Identifier& special_ident) {
  putf("* Specialized identifier");
  putf("- {}", special_ident.name);
  putf("- Types:");
  self->print_types(special_ident.specializations);
}

void AstPrinter::print_expr(Ast_Path& path) {
  putf("* Path");
  size_t access_order_no = 0;
  for (auto& p: path.path) {
    indent();
    putf("{}. {}", access_order_no, p.name);
    ++access_order_no;
  }
}

void AstPrinter::print_switch_cases(std::vector<SwitchCase>& cases) {
  for (auto& switch_case: cases) {
    if (!switch_case.is_default_case) {
      putf("- Case:");
      self->print_expr(*switch_case.match);
    } else {
      putf("- Default case:");
    }
    if (switch_case.bindings) {
      putf("- Bindings:");
      self->print_binding(*switch_case.bindings);
    }
    putf("- Body:");
    self->print_expr(switch_case.body);
  }
}

void AstPrinter::print_expr(Ast_Switch_Expr& switch_expr) {
  putf("* Switch expr");
  self->print_switch_cases(switch_expr.cases);
}
