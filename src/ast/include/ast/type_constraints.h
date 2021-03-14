#pragma once

#include "ast/node.h"

struct AstContext;
struct Ast_Field_Access;
struct Ast_Identifier;
DEF_TYPE(TypeFieldConstraint) {
  Type_Ptr type;

  // unsafe ctor
  TypeFieldConstraint(
    Expr_Ptr object, Ast_Identifier const & field, int& field_index,
    Expression_Meta::ValueType* value_type = nullptr);

  TypeFieldConstraint(Ast_Field_Access& access);

  static Type_Ptr get(AstContext& ctx, Ast_Field_Access& access);
  static Type_Ptr get(AstContext& ctx,
    Expr_Ptr object, Ast_Identifier const & field, int& field_index);

  inline Expr_Ptr get_object() const { return field_access.object; }
  inline Ast_Identifier const & get_field() const { return *field_access.field; }
  inline Expression_Meta::ValueType const * get_value_type() const {
    return field_access.value_type;
  };

  inline void resolve_field_index(int field_index) const {
    *field_access.field_index = field_index;
  }

  inline bool resolve_value_type(Expression_Meta::ValueType value_type) {
    if (field_access.value_type) {
      *field_access.value_type = value_type;
      return true;
    }
    return false;
  }

private:
  struct FieldAccess {
    Expr_Ptr object;
    // These references must be to members of a AST node (i.e. stay alive)
    // (have to be pointers or makes issues with constructing types)
    Ast_Identifier const * field;
    int* field_index;
    Expression_Meta::ValueType* value_type = nullptr;
  } field_access;
};

struct Ast_Index_Access;
DEF_TYPE(TypeIndexConstraint) {
  Type_Ptr type;
  SpAstPtr<Ast_Expression, Ast_Index_Access> index_access;

  static Type_Ptr get(AstContext& ctx, Ast_Index_Access& access);
};

struct Ast_As_Cast;
DEF_TYPE(TypeCastConstraint) {
  Type_Ptr type;
  SpAstPtr<Ast_Expression, Ast_As_Cast> as_cast;

  static Type_Ptr get(AstContext& ctx, Ast_As_Cast& as_cast);
};

DEF_TYPE(LValueConstraint) {
  Expr_Ptr expected_lvalue;

  static Type_Ptr get(AstContext& ctx, Expr_Ptr expected_lvalue);
};

DEF_TYPE(SwitchableConstraint) {
  Type_Ptr type;
  Expr_Ptr switched;

  static Type_Ptr get(AstContext& ctx, Expr_Ptr switched);
};
