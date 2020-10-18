#pragma once

#include "ast/ast.h"

namespace AstHelper {

bool check_reachability(Ast_Block& block, Ast_Statement** unreachable_stmt);
bool check_reachability(Ast_Expression& block_like, Ast_Statement** unreachable_stmt);
bool check_reachability(Ast_Statement& statement, Ast_Statement** unreachable_stmt);

}
