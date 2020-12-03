
#include "ast/ast.h"
#include "ast/context.h"

struct ContextData {
  // vectors that own the types/exprs/stmts should not be messed with
  std::vector<Type> types;
  std::vector<Ast_Statement> stmts;
  std::vector<Ast_Expression> exprs;

  template <typename T>
  AstPtr<T> add(T node, std::vector<T>& nodes) {
    AstPtr<T> node_ptr(&nodes, nodes.size());
    std::visit([&](auto& node){
      node.self = node_ptr;
    }, node.v);
    nodes.push_back(node);
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
