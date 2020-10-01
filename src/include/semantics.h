#pragma once

#include "ast.h"

namespace Sema {

void analyse_file(Ast_File& file);
void analyse_function_header(Ast_Function_Declaration& func);
void analyse_function_body(Ast_Function_Declaration& func);
void analyse_statement(Ast_Statement& stmt, Scope& scope, Type* return_type = nullptr);

Type_Ptr analyse_expression(Ast_Expression& expr, Scope& scope);
Type_Ptr analyse_binary_expression(Ast_Binary_Operation& expr, Scope& scope);
Type_Ptr analyse_call(Ast_Call& expr, Scope& scope);

}
