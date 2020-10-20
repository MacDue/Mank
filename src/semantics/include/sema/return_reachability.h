#pragma once

#include "ast/ast.h"

namespace AstHelper {

bool all_paths_return(Ast_Block& block, Ast_Statement** unreachable_stmt);
bool all_paths_return(Ast_Expression& block_like, Ast_Statement** unreachable_stmt);
bool all_paths_return(Ast_Statement& statement, Ast_Statement** unreachable_stmt);

}
