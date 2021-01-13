#pragma once

#include "ast/self_helper.h"

DEF_TYPE(TypeVar) {
  int32_t id;
  // OR
  enum Constraint {
    NUMERIC = -1, // any int or float
    INTEGER = -2, // any int
    ADDABLE = -3, // any int, float or string
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

  static Type_Ptr get(Constraint constraint);
};
