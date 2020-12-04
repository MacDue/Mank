#pragma once

#include "ast/self_helper.h"

DEF_TYPE(TypeVar) {
  int32_t id;
  bool is_return_type = false;
  // OR
  enum Constraint {
    NUMERIC = -1, // any int or float
    INTEGER = -2, // any int
  };

  static int32_t constexpr ANY = -42; // magic id for any tvar

  explicit TypeVar(int32_t id): id{id} {}

  TypeVar() {
    static auto next_id = 0;
    id = next_id++;
  }

  TypeVar(Constraint constraint)
    : id{static_cast<int32_t>(constraint)} {}

  inline bool special() const { return id < 0; }
  inline bool operator <(TypeVar const & other) const {
    return this->id < other.id;
  }

  // static get_field_constraint(std::string name)

  static Type_Ptr integer();
  static Type_Ptr numeric();
  static Type_Ptr get(Constraint constraint);
};

struct Ast_Field_Access;

DEF_TYPE(TypeFieldConstraint) {
  Type_Ptr type;
  // Not null just can't be a ref
  SpAstPtr<Ast_Expression, Ast_Field_Access> field_access;

  static Type_Ptr get(Ast_Field_Access& access);
};
