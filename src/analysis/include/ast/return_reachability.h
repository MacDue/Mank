#pragma once

#include "ast.h"

namespace AstHelper {

bool check_reachability(Ast_Statement& block_like, Ast_Statement** unreachable_stmt);

}
