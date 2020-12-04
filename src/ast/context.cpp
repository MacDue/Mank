#include <deque>
#include "ast/ast.h"
#include "ast/context.h"

struct ContextData {
  // vectors that own the types/exprs/stmts should not be messed with
  // You could probably do something much better than vectors but it'll do for now.
  std::deque<Type> types;
  std::deque<Ast_Statement> stmts;
  std::deque<Ast_Expression> exprs;

  template <typename T>
  AstPtr<T> add(T node, std::deque<T>& nodes) {
    nodes.push_back(node);
    auto& new_node = nodes.back();
    AstPtr<T> node_ptr(&new_node);
    std::visit([&](auto& node){
      node.self = node_ptr;
    }, new_node.v);
    return node_ptr;
  }
};

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

AstContext::~AstContext() {}
