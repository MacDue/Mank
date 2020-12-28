#pragma once

#include <string>
#include <utility>
#include <formatxx/std_string.h>

#include "ast/ast.h"

/* -- Constructs -- */

struct AstBuilder {
  Ast_File& file;
  AstBuilder(Ast_File& file)
    : file{file} {}

static inline Ast_File new_file() {
  Ast_File file("<built_ast>");
  return file;
}

template <typename TStmt>
Stmt_Ptr to_stmt_ptr(TStmt && stmt) {
  return file.ctx.new_stmt(stmt);
}

template <typename TExpr>
Expr_Ptr to_expr_ptr(TExpr && expr) {
  return file.ctx.new_expr(expr);
}

template <typename T>
Type_Ptr to_type_ptr(T && type) {
  return file.ctx.new_type(type);
}

template<typename... TFunction>
Ast_File& add_functions(TFunction && ... function) {
  if constexpr (sizeof...(function) > 0) {
    file.functions.push_back(to_type_ptr(function)...);
  }
  return file;
}

/* Args */

inline Ast_Argument make_argument(Type_Ptr type, std::string name) {
  return Ast_Argument {
    .type = type,
    .name = Ast_Identifier(name)
  };
}

inline Ast_Argument make_argument(std::string name) {
  return Ast_Argument {
    .type = file.ctx.new_tvar(),
    .name = Ast_Identifier(name)
  };
}

template<typename... TArgs>
std::vector<Ast_Argument> make_args(TArgs && ... args) {
  return std::vector<Ast_Argument>{ args... };
}

/* Types */

inline Type_Ptr make_unchecked_type(std::string type_name) {
  UncheckedType unchecked(Ast_Identifier{type_name});
  return to_type_ptr(unchecked);
}

inline Type_Ptr make_reference(Type_Ptr type) {
  assert(!is_reference_type(type));
  ReferenceType ref_type;
  ref_type.references = type;
  return to_type_ptr(ref_type);
}

template <typename... TFields>
Type_Ptr make_pod(std::string name, TFields && ... fields) {
  Ast_Pod_Declaration pod;
  pod.identifier.name = name;
  pod.fields = std::vector<Ast_Argument> { fields... };
  return to_type_ptr(pod);
}

/* Functions */

inline Ast_Function_Declaration make_function(
  bool procedure,
  Type_Ptr return_type,
  std::string name,
  std::vector<Ast_Argument>&& args,
  Ast_Block&& body
) {
  Ast_Function_Declaration function;
  function.identifier.name = name;
  function.procedure = procedure;
  function.return_type = return_type;
  function.arguments = args;
  function.body = body;
  return function;
}

inline Ast_Function_Declaration make_function(
  Type_Ptr return_type,
  std::string name,
  std::vector<Ast_Argument>&& args,
  Ast_Block&& body
) {
  return make_function(false, return_type, name, std::move(args), std::move(body));
}

inline Ast_Function_Declaration make_procedure(
  std::string name,
  std::vector<Ast_Argument>&& args,
  Ast_Block&& body
) {
  return make_function(true, nullptr, name, std::move(args), std::move(body));
}

inline Ast_Function_Declaration make_function(
  Type_Ptr return_type,
  std::string name,
  Ast_Block&& body
) {
  return make_function(return_type, name, {}, std::move(body));
}

inline Ast_Function_Declaration make_procedure(
  std::string name,
  Ast_Block&& body
) {
  return make_procedure(name, {}, std::move(body));
}

/* Blocks */

template <typename... TStmt>
static Ast_Block make_body(bool has_final_expr, TStmt && ... stmts) {
  Ast_Block block;
  block.statements = std::vector<Stmt_Ptr>{ stmts... };
  block.has_final_expr = has_final_expr;
  return block;
}

template <typename... TStmt>
Expr_Ptr make_block(bool has_final_expr, TStmt && ... stmts) {
  return to_expr_ptr(make_body(has_final_expr, stmts...));
}

template <typename... TStmt>
Ast_Block make_stmt_body(TStmt && ... stmts) {
  return make_body(false, stmts...);
}

template <typename... TStmt>
Expr_Ptr make_stmt_block(TStmt && ... stmts) {
  return make_block(false, stmts...);
}

/* Types */

inline Type_Ptr make_type(std::string name) {
  UncheckedType type(Ast_Identifier{name});
  return to_type_ptr(type);
}

/* -- Expressions -- */

/* If expressions */

inline Expr_Ptr make_if(
  Expr_Ptr cond, Expr_Ptr then_block, Expr_Ptr else_block = nullptr
) {
  Ast_If_Expr if_stmt;
  if_stmt.cond = cond;
  if_stmt.then_block = then_block;
  if (else_block) {
    if_stmt.has_else = true;
    if_stmt.else_block = else_block;
  }
  return to_expr_ptr(if_stmt);
}

/* Literals */

inline Expr_Ptr make_literal(PrimativeType::Tag type, std::string value) {
  Ast_Literal literal;
  literal.literal_type = type;
  literal.value = value;
  return to_expr_ptr(literal);
}

inline Expr_Ptr make_string(std::string value) {
  return make_literal(PrimativeType::STRING, value);
}

inline Expr_Ptr make_integer(int value) {
  return make_literal(PrimativeType::INTEGER, std::to_string(value));
}

inline Expr_Ptr make_float64(double value) {
  return make_literal(PrimativeType::FLOAT64, std::to_string(value));
}

inline Expr_Ptr make_boolean(bool value) {
  return make_literal(PrimativeType::BOOL, value ? "true" : "false");
}

/* Idents */

inline Expr_Ptr make_ident(std::string name) {
  Ast_Identifier ident(name);
  return to_expr_ptr(ident);
}

inline Expr_Ptr make_macro_ident(std::string name) {
  Ast_Macro_Identifier ident;
  ident.name = name;
  return to_expr_ptr(ident);
}

/* Calls */

template <typename... TArgs>
Expr_Ptr make_call(Expr_Ptr callee, TArgs && ... args) {
  Ast_Call call;
  call.callee = callee;
  call.arguments = std::vector<Expr_Ptr> { args ... };
  return to_expr_ptr(call);
}

template <typename... TArgs>
Expr_Ptr make_call(std::string callee, TArgs && ... args) {
  return make_call(make_ident(callee), args...);
}

/* Unary */

inline Expr_Ptr make_unary(Ast_Operator op, Expr_Ptr operand) {
  Ast_Unary_Operation unary;
  unary.operation = op;
  unary.operand = operand;
  return to_expr_ptr(unary);
}

/* Binary */

inline Expr_Ptr make_binary(
  Ast_Operator op, Expr_Ptr left, Expr_Ptr right
) {
  Ast_Binary_Operation binop;
  binop.operation = op;
  binop.left = left;
  binop.right = right;
  return to_expr_ptr(binop);
}

/* Field access */

inline Expr_Ptr make_access(Expr_Ptr object, std::string field) {
  Ast_Field_Access access;
  access.object = object;
  access.field.name = field;
  return to_expr_ptr(access);
}

/* Array/tuple literals */

template <typename TAgg, typename... TElements>
inline Expr_Ptr make_expr_list(TElements && ... elements) {
  TAgg agg;
  agg.elements = std::vector<Expr_Ptr> { elements... };
  return to_expr_ptr(agg);
}

template <typename... TElements>
inline Expr_Ptr make_array(TElements && ... elements) {
  return make_expr_list<Ast_Array_Literal>(elements...);
}

template <typename... TElements>
inline Expr_Ptr make_tuple(TElements && ... elements) {
  return make_expr_list<Ast_Tuple_Literal>(elements...);
}

/* Index access */

inline Expr_Ptr make_index(Expr_Ptr object, Expr_Ptr index) {
  Ast_Index_Access access;
  access.object = object;
  access.index = index;
  return to_expr_ptr(access);
}

/* Lambda expr */

inline Expr_Ptr make_lambda(
  Type_Ptr return_type,
  std::vector<Ast_Argument>&& args,
  Ast_Block&& body
) {
  Ast_Lambda lambda;
  lambda.arguments = args;
  lambda.body = body;
  lambda.return_type = return_type;
  return to_expr_ptr(lambda);
}

inline Expr_Ptr make_lambda(
  std::vector<Ast_Argument>&& args,
  Ast_Block&& body
) {
  return make_lambda(file.ctx.new_tvar(), std::move(args), std::move(body));
}

/* Pod literal */

inline PodFieldInitializer make_field_init(
  std::string field_name, Expr_Ptr initializer
) {
  return PodFieldInitializer {
    .field = Ast_Identifier(field_name),
    .initializer = initializer
  };
}

template <typename... TFieldInits>
inline Expr_Ptr make_pod_init(
  std::string pod_name,
  TFieldInits && ... field_inits
) {
  Ast_Pod_Literal pod_literal;
  pod_literal.pod = make_type(pod_name);
  pod_literal.fields = std::vector<PodFieldInitializer> { field_inits... };
  return to_expr_ptr(pod_literal);
}

/* -- Statements -- */

/* Expression statement */

inline Stmt_Ptr make_expr_stmt(Expr_Ptr expr, bool final_expr = false) {
  Ast_Expression_Statement expr_stmt;
  expr_stmt.expression = expr;
  expr_stmt.final_expr = final_expr;
  return to_stmt_ptr(expr_stmt);
}

/* Return statement */

inline Stmt_Ptr make_return(Expr_Ptr value) {
  Ast_Return_Statement return_stmt;
  return_stmt.expression = value;
  return to_stmt_ptr(return_stmt);
}

/* If statement */

inline Stmt_Ptr make_if_stmt(
  Expr_Ptr cond, Expr_Ptr then_block, Expr_Ptr else_block = nullptr
) {
  return make_expr_stmt(make_if(cond, then_block, else_block));
}

/* Variable declarations */

inline Stmt_Ptr make_var_decl(
  std::string name,
  Type_Ptr type = nullptr,
  Expr_Ptr initializer = nullptr
) {
  Ast_Variable_Declaration var_decl;
  var_decl.type = type;
  var_decl.variable.name = name;
  var_decl.initializer = initializer;
  return to_stmt_ptr(var_decl);
}

inline Stmt_Ptr make_var_decl(
  std::string name,
  Expr_Ptr initializer
) {
  return make_var_decl(name, nullptr, initializer);
}

/* Assignment statements */

inline Stmt_Ptr make_assignment(
  Expr_Ptr target, Expr_Ptr value
) {
  Ast_Assign assign;
  assign.target = target;
  assign.expression = value;
  return to_stmt_ptr(assign);
}

/* For loops */

inline Stmt_Ptr make_for(
  std::string loop_value, Type_Ptr value_type,
  Expr_Ptr start_range, Expr_Ptr end_range,
  Ast_Block loop_body
) {
  Ast_For_Loop for_loop;
  for_loop.loop_variable.name = loop_value;
  for_loop.type = value_type;
  for_loop.start_range = start_range;
  for_loop.end_range = end_range;
  for_loop.body = loop_body;
  return to_stmt_ptr(for_loop);
}

inline Stmt_Ptr make_for(
  std::string loop_value,
  Expr_Ptr start_range, Expr_Ptr end_range,
  Ast_Block loop_body
) {
  return make_for(loop_value, nullptr, start_range, end_range, loop_body);
}

template <typename... TBindings>
inline TupleBinding make_tuple_binding(
  TBindings && ... bindings
) {
  TupleBinding binding;
  (binding.binds.push_back(bindings), ...);
  return binding;
}

inline Stmt_Ptr make_bind(
  TupleBinding bindings, Expr_Ptr initializer
) {
  Ast_Tuple_Structural_Binding tuple_binding;
  tuple_binding.bindings = bindings;
  tuple_binding.initializer = initializer;
  return to_stmt_ptr(tuple_binding);
}

/* Test helpers */

inline Ast_File& wrap_stmt(Stmt_Ptr stmt, bool has_final_expr = false) {
  add_functions(make_procedure("test", make_body(has_final_expr, stmt)));
  return file;
}

inline Ast_File& wrap_expr(Expr_Ptr expr, bool final_expr = false) {
  return wrap_stmt(make_expr_stmt(expr), final_expr);
}

inline Ast_File& wrap_final_expr(Expr_Ptr expr) {
  return wrap_expr(expr, true);
}

/* Misc */

template<typename T>
static Ast_Identifier to_internal_name(T stringifyable) {
  return Ast_Identifier(formatxx::format_string("!{}", stringifyable));
}

};
