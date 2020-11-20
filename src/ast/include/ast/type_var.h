#pragma once

#include "defs.h"

struct TypeVar {
  int32_t id;
  // OR
  enum Constraint {
    NUMERIC = -1, // any int or float
    INTEGER = -2, // any int
  };

  /*
    Knowing it's own containing type pointer makes it
    easy to apply the solved types back onto the AST without walking it.
  */
  std::weak_ptr<Type> substitute;

  inline static const Constraint Constraints[] = { NUMERIC, INTEGER };

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

  static Type_Ptr integer();
  static Type_Ptr numeric();
  static Type_Ptr get(Constraint constraint);
};
