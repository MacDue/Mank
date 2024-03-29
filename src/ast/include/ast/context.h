#pragma once

#include <set>
#include <memory>

#include "ast/node.h"
#include "ast/type_var.h"

struct ContextData;

class AstContext {
  std::unique_ptr<ContextData> data;

  Type_Ptr add_type(Type type);
  Stmt_Ptr add_stmt(Ast_Statement type);
  Expr_Ptr add_expr(Ast_Expression type);
  Item_Ptr add_item(Ast_Item item);
public:
  AstContext();
  AstContext(AstContext&& ctx);

  /*
    Only for types like i32, f64, etc.
    Not valid for non-static ptrs.
    (unless they are not freed-but new_type should do that).
  */
  static Type_Ptr make_static_type_ptr(Type* static_ptr);

  template <typename T>
  Type_Ptr new_type(T type) {
    return add_type(Type(type));
  }

  inline Type_Ptr new_tvar() {
    return new_type(TypeVar());
  }

  template <typename T>
  Stmt_Ptr new_stmt(T stmt) {
    return add_stmt(Ast_Statement(stmt));
  }

  template <typename T>
  Expr_Ptr new_expr(T expr) {
    return add_expr(Ast_Expression(expr));
  }

  template <typename T>
  Item_Ptr new_item(T item) {
    return add_item(Ast_Item(item));
  }

  template <typename Type, typename Item>
  Type_Ptr new_identified_type(Item& item) {
    // Creates a new (empty) type linked to a top level item (e.g. Pod)
    Type type;
    type.identifier = item.identifier;
    return item.get_self().class_ptr()->declared_type = new_type(type);
  }

  ~AstContext();
};
