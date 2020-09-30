#pragma once

#include <vector>
#include <memory>
#include <variant>

#include "scope.h"
#include "types.h"
#include "constants.h"
#include "variant_meta.h"
#include "literal_type.h"
#include "source_location.h"

/* Top level constructs */
struct Ast_File;
struct Ast_Function_Declaration;

/* Expressions */
struct Ast_Expression;
struct Ast_Call;
struct Ast_Number;
struct Ast_String;
struct Ast_Identifier;
struct Ast_Unary_Operation;
struct Ast_Binary_Operation;

/* Statements */
struct Ast_Statement;
struct Ast_Block;
struct Ast_If_Statement;
struct Ast_Expression_Statement;
struct Ast_Return_Statement;

using Statement_Ptr = std::shared_ptr<Ast_Statement>;
using Expression_Ptr = std::shared_ptr<Ast_Expression>;

/* Test AST based on std::variant & pattern matching */

struct Ast_Node {
  SourceLocation location;

  Ast_Node() = default;
  Ast_Node(SourceLocation location): location{location} {}
};

enum class Ast_Operator {
  #include "operators.def"
};

struct Ast_Identifier: Ast_Node {
  std::string name;

  Ast_Identifier() = default;
  Ast_Identifier(SourceLocation location, std::string name)
    : Ast_Node(location), name{name} {}
};

struct Ast_Argument {
  std::shared_ptr<Type> type;
  Ast_Identifier name;
};

struct Ast_File: Ast_Node {
  Scope scope;
  std::vector<
    std::shared_ptr<Ast_Function_Declaration>> functions;
};

/* Expressions */

struct Ast_Call: Ast_Node {
  Expression_Ptr callee;
  std::vector<Expression_Ptr> arguments;
};

struct Ast_Literal: Ast_Node {
  LiteralType literal_type;
  std::string value;

  // Ast_Literal(
  //   SourceLocation location, LiteralType literal_type, std::string value
  // ) : Ast_Node(location), literal_type{literal_type}, value{value} {}
};

struct Ast_Unary_Operation: Ast_Node {
  Expression_Ptr operand;
  Ast_Operator operation;
};

struct Ast_Binary_Operation: Ast_Node {
  Expression_Ptr left, right;
  Ast_Operator operation;
  bool parenthesised = false;
};

struct Ast_Expression: std::variant<
  Ast_Call,
  Ast_Literal,
  Ast_Identifier,
  Ast_Unary_Operation,
  Ast_Binary_Operation
> {
  using variant::variant;
};

VARIANT_META_FOR(Ast_Expression)

/* Statements */

struct Ast_Expression_Statement: Ast_Node {
  Expression_Ptr expression;

  // Ast_Expression_Statement(SourceLocation location, Expression_Ptr expr)
  //   : Ast_Node(location), expression{expr} {}
};

struct Ast_Return_Statement: Ast_Node {
  Expression_Ptr expression;

  // Ast_Return_Statement(SourceLocation location, Expression_Ptr expr)
  //   : Ast_Node(location), expression{expr} {}
};

struct Ast_Block: Ast_Node {
  Scope scope;
  std::vector<Statement_Ptr> statements;

  // Ast_Block(SourceLocation location, std::vector<Statement_Ptr> stmts)
  //   : Ast_Node(location), statements{stmts} {};
};

struct Ast_If_Statement: Ast_Node {
  bool has_else = false;
  Expression_Ptr cond;
  Statement_Ptr then_block, else_block;
};

struct Ast_Statement: std::variant<
  Ast_Expression_Statement,
  Ast_Return_Statement,
  Ast_Block,
  Ast_If_Statement
> {
  using variant::variant;
};

VARIANT_META_FOR(Ast_Statement)

/* Top level constructs */

struct Ast_Function_Declaration: Ast_Node, Type /* will need to change */ {
  bool external = false;
  bool c_function = false;
  bool procedure = false;
  Ast_Identifier identifer;
  std::vector<Ast_Argument> arguments;
  std::shared_ptr<Type> return_type;
  Ast_Block body;
};
