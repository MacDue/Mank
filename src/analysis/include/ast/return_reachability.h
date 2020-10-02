#pragma once

#include "ast.h"

namespace AstHelper {

bool is_return_block(
  Ast_Block& block,
  Ast_Return_Statement** out_return_stmt = nullptr);

bool block_returns(
  Ast_Statement& block_like,
  Ast_Return_Statement** out_return_stmt = nullptr,
  bool only_last = false);

}
