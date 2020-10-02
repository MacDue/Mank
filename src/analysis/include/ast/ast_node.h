#pragma once

#include <memory>
#include <variant>

#include "types.h"
#include "constants.h"
#include "source_location.h"

/* Top level constructs */
struct Ast_File;
struct Ast_Function_Declaration;

/* Expressions */
struct Ast_Expression;
struct Ast_Call;
struct Ast_Literal;
struct Ast_Identifier;
struct Ast_Unary_Operation;
struct Ast_Binary_Operation;

/* Statements */
struct Ast_Statement;
struct Ast_Block;
struct Ast_If_Statement;
struct Ast_Expression_Statement;
struct Ast_Return_Statement;

/* Types */
struct UncheckedType;
struct PrimativeType;

using Statement_Ptr = std::shared_ptr<Ast_Statement>;
using Expression_Ptr = std::shared_ptr<Ast_Expression>;
using Function_Ptr = Type_Ptr;

/* Test AST based on std::variant & pattern matching */

struct Ast_Node {
  SourceLocation location;

  Ast_Node() = default;
  Ast_Node(SourceLocation location): location{location} {}
};

struct Ast_Identifier: Ast_Node {
  std::string name;

  Ast_Identifier() = default;
  Ast_Identifier(SourceLocation location, std::string name)
    : Ast_Node(location), name{name} {}
  Ast_Identifier(std::string name)
    : Ast_Identifier({} /* dummy */, name) {};
};

enum class Ast_Operator {
  #include "operators.def"
};

struct Ast_Argument {
  Type_Ptr type;
  Ast_Identifier name;
};
