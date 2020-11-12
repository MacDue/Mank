#pragma once

#include <memory>
#include <variant>
#include <cassert>

#include "parser/constants.h"

#include "ast/primative_types.h"
#include "ast/source_location.h"

/* Top level constructs */
struct Ast_File;
struct Ast_Function_Declaration;
struct Ast_Pod_Declaration;

/* Expressions */
struct Ast_Expression;
struct Ast_Block;
struct Ast_Call;
struct Ast_Literal;
struct Ast_Identifier;
struct Ast_Unary_Operation;
struct Ast_Binary_Operation;

/* Statements */
struct Ast_Statement;
struct Ast_If_Statement;
struct Ast_Expression_Statement;
struct Ast_Return_Statement;

/* Types */
struct Type;
struct UncheckedType;
struct PrimativeType;

using Type_Ptr = std::shared_ptr<Type>;
using Statement_Ptr = std::shared_ptr<Ast_Statement>;
using Expression_Ptr = std::shared_ptr<Ast_Expression>;
using Function_Ptr = Type_Ptr;

/* Test AST based on std::variant & pattern matching */

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
  std::weak_ptr<Type> type;

  // If this expression can appear on the left of an assignment
  enum ValueType {
    LVALUE,
    RVALUE
  } value_type = RVALUE;

  /*
    Type the expression does own.
    For example an array literal owns it's type.
    (this does lead to some duplicate types)

    This is just a quick workaround as something has to own the pointer.
    A better solution would probably be to have some from of "context"
    (like llvm does that can create or return types)
  */
  Type_Ptr owned_type = nullptr;

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

class Ast_Expression_Node {
  Expression_Meta* meta = nullptr;
  friend class Ast_Expression;
public:
  inline Expression_Meta& get_meta() {
    assert(meta != nullptr && "fix me! expression missing needed meta data");
    return *meta;
  }

  inline auto get_type() {
    return get_meta().type;
  }

  inline void update_type(Type_Ptr type) {
    get_meta().type = type;
  }

  inline PrimativeValue const_value() {
    return get_meta().const_value;
  }

  inline void update_const_value(PrimativeValue value) {
    get_meta().const_value = value;
  }
};

#define Expression_Node Ast_Node, Ast_Expression_Node

struct Ast_Identifier: Expression_Node {
  std::string name;

  Ast_Identifier() = default;
  Ast_Identifier(SourceLocation location, std::string name)
    : Ast_Node(location), name{name} {}
  Ast_Identifier(std::string name)
    : Ast_Identifier({} /* dummy */, name) {};
};

struct Ast_Argument {
  Type_Ptr type;
  Ast_Identifier name;
};
