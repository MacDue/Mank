#pragma once

#include <memory>

/* Top level constructs */
struct Ast_File;
struct Ast_Function_Declaration;
struct Ast_Pod_Declaration;
struct Ast_Enum_Declaration;
struct Ast_Constant_Declaration;

/* Expressions */
struct Ast_Expression;
struct Ast_Block;
struct Ast_Call;
struct Ast_Literal;
struct Ast_Identifier;
struct Ast_Unary_Operation;
struct Ast_Binary_Operation;
struct Ast_Path;

/* Statements */
struct Ast_Statement;
struct Ast_If_Statement;
struct Ast_Expression_Statement;
struct Ast_Return_Statement;

/* Types */
struct Type;
struct UncheckedType;
struct PrimativeType;
