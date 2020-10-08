#include <cassert>
#include <mpark/patterns.hpp>

#include "ast/return_reachability.h"

namespace AstHelper {

#define LAST_UNREACHABLE_STMT(stmt)   \
  if (unreachable_stmt != nullptr) {  \
      *unreachable_stmt = &*(stmt);   \
  }

bool check_reachability(Ast_Statement& block_like, Ast_Statement** unreachable_stmt) {
  using namespace mpark::patterns;
  return match(block_like.v)(
    pattern(as<Ast_Block>(arg)) = [&](auto& block) {
      bool block_returns = false;
      for (uint stmt_idx = 0; stmt_idx < block.statements.size(); stmt_idx++) {
        auto& stmt = block.statements.at(stmt_idx);
        block_returns |= check_reachability(*stmt, unreachable_stmt);
        if (block_returns && stmt_idx + 1 < block.statements.size()) {
          LAST_UNREACHABLE_STMT(block.statements.at(stmt_idx + 1));
        }
      }
      return block_returns;
    },
    pattern(as<Ast_If_Statement>(arg)) = [&](auto& if_stmt) {
      return std::visit([&](Ast_Const_Expr& const_expr){
        if (const_expr.is_const_expr()) {
          bool if_returns;
          // If the if cond is a constant expression the reachablity should only
          // be checked in the reachable branch for better errors
          bool cond_value = std::get<bool>(const_expr.const_expr_value);
          if (cond_value) {
            if_returns = check_reachability(*if_stmt.then_block, unreachable_stmt);
            if (if_stmt.has_else) {
              LAST_UNREACHABLE_STMT(if_stmt.else_block);
            }
          } else {
            if_returns = if_stmt.has_else && check_reachability(*if_stmt.else_block, unreachable_stmt);
            LAST_UNREACHABLE_STMT(if_stmt.then_block);
          }
          return if_returns;
        } else {
          bool then_returns = check_reachability(*if_stmt.then_block, unreachable_stmt);
          if (!if_stmt.has_else) {
            return false; // Not _all_ paths return since it does not have an else;
          }
          return then_returns && check_reachability(*if_stmt.else_block, unreachable_stmt);
        }
      }, if_stmt.cond->v);
    },
    pattern(as<Ast_Return_Statement>(_)) = []{ return true; },
    pattern(_) = []{ return false; });
}

}
