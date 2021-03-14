#pragma once

#include <memory>
#include <variant>
#include <cassert>

#include "parser/constants.h"

#include "ast/primative_types.h"
#include "ast/source_location.h"

#include "ast/self_helper.h"

struct Ast_Node {
  SourceLocation location;

  Ast_Node() = default;
  Ast_Node(SourceLocation location): location{location} {}
};

enum class Ast_Operator {
  #include "parser/operators.def"
};

/* Expression meta stuff */

struct Expression_Meta {
  /*
    The expression must not own it's resolved type or it can create cycles!

    for example in a recursive function:

    fun wee: i32 {
      return wee(); # the callee type will be `fun wee: i32``
                    # that ast node is within wee so we have a cycle.
    }

    I _think_ all will be owned by something else:
      - Primatives (static data)
      - Functions (owned by function node)
      - More complex types will probably be nodes within the AST already
  */
  Type_Ptr type;

  // If this expression can appear on the left of an assignment
  enum ValueType {
    LVALUE,
    RVALUE
  } value_type = RVALUE;

  PrimativeValue const_value;

  inline bool is_const() {
    return !std::holds_alternative<std::monostate>(const_value);
  }

  PrimativeValue* get_const_value() {
    if (is_const()) {
      return &const_value;
    }
    return nullptr;
  }
};

template <typename T>
class Ast_Expression_Node: public AstSelf<Ast_Expression, T> {
  friend class Ast_Expression;
public:
  inline Expression_Meta& get_meta() {
    return this->get_self().class_ptr()->meta;
  }

  inline Expression_Meta const & get_meta() const {
    return this->get_self().class_ptr()->meta;
  }

  inline auto get_type() {
    return get_meta().type;
  }

  inline void update_type(Type_Ptr type) {
    get_meta().type = type;
  }

  inline PrimativeValue const_value() const {
    return get_meta().const_value;
  }

  inline void update_const_value(PrimativeValue value) {
    get_meta().const_value = value;
  }
};

DEF_EXPR(Ast_Identifier) {
  std::string name;

  inline bool empty() const { return name.empty(); }

  Ast_Identifier() = default;
  Ast_Identifier(SourceLocation location, std::string name)
    : Ast_Node(location), name{name} {}
  Ast_Identifier(std::string name)
    : Ast_Identifier({} /* dummy */, name) {};

  bool operator < (Ast_Identifier const & other) const {
    return name < other.name;
  }
};

// foo::bar::baz
DEF_EXPR(Ast_Path) {
  bool leading_colons = false;
  std::vector<Ast_Identifier> path;
};

struct Ast_Argument {
  Type_Ptr type;
  Ast_Identifier name;
};
