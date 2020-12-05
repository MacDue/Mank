#include <deque>
#include "ast/ast.h"
#include "ast/context.h"
#include "ast/expr_helpers.h"

struct ContextData {
  // vectors that own the types/exprs/stmts should not be messed with
  // You could probably do something much better than vectors but it'll do for now.
  std::deque<Type> types;
  std::deque<Ast_Statement> stmts;
  std::deque<Ast_Expression> exprs;

  template <typename T>
  static AstPtr<T> init_node(T& new_node) {
    AstPtr<T> node_ptr(&new_node);
    std::visit([&](auto& node){
      node.self = node_ptr;
    }, new_node.v);
    return node_ptr;
  }

  template <typename T>
  static AstPtr<T> add(T node, std::deque<T>& nodes) {
    nodes.push_back(node);
    return init_node(nodes.back());
  }
};

Type_Ptr AstContext::make_static_type_ptr(Type* static_ptr) {
  return ContextData::init_node(*static_ptr);
}

AstContext::AstContext(): data{std::make_unique<ContextData>()} {}
AstContext::AstContext(AstContext&& ctx): data{std::move(ctx.data)} {}

Type_Ptr AstContext::add_type(Type type) {
  return data->add(type, data->types);
}

Stmt_Ptr AstContext::add_stmt(Ast_Statement stmt) {
  return data->add(stmt, data->stmts);
}

Expr_Ptr AstContext::add_expr(Ast_Expression expr) {
  return data->add(expr, data->exprs);
}

void AstHelper::rewrite_expr(Ast_Expression& expr, Ast_Expression_Type rewrite) {
  expr.v = rewrite; // now the self pointer in rewrite is invalid!;
  ContextData::init_node(expr); // fixed!
}

AstContext::~AstContext() {}
