#include <cassert>
#include <mpark/patterns.hpp>

#include "ast/return_reachability.h"

namespace AstHelper {

#define LAST_UNREACHABLE_STMT(stmt)   \
  if (unreachable_stmt != nullptr) {  \
      *unreachable_stmt = &*(stmt);   \
  }

bool check_reachability(Ast_Statement& block_like, Ast_Statement** unreachable_stmt) {
  // using namespace mpark::patterns;
  // return match(block_like.v)(
  //   pattern(as<Ast_Block>(arg)) = [&](auto& block) {
  //     bool block_returns = block.final_expr != nullptr;
  //     for (uint stmt_idx = 0; stmt_idx < block.statements.size(); stmt_idx++) {
  //       auto& stmt = block.statements.at(stmt_idx);
  //       block_returns |= check_reachability(*stmt, unreachable_stmt);
  //       if (block_returns && stmt_idx + 1 < block.statements.size()) {
  //         LAST_UNREACHABLE_STMT(block.statements.at(stmt_idx + 1));
  //       }
  //     }
  //     return block_returns;
  //   },
  //   pattern(as<Ast_If_Statement>(arg)) = [&](auto& if_stmt) {
  //     return std::visit([&](Ast_Const_Expr& const_expr){
  //       /*
  //         When deciding if a if statement returns the constant evaluation should not
  //         be used. As it can make it confusing, as when the constant eval improves,
  //         previously rejected functions can be accepted.

  //         Things can also get arbitrarily complex.
  //         e.g.

  //         fun fermats_last_theorm: bool (x: i32, y: i32, z: i32, n: i32) {
  //           if n > 2 {
  //             if x^n + y^n == z^n {
  //               # A sufficiently advanced compiler could prove this is
  //               # unreachable and hence not matter there's no return.
  //             }
  //             return false;
  //           } else {
  //             return true;
  //           }
  //         }
  //       */

  //       // If can only return on _all_ paths if it has an else block, which also returns.
  //       bool if_returns = check_reachability(*if_stmt.then_block, unreachable_stmt)
  //           && if_stmt.has_else && check_reachability(*if_stmt.else_block, unreachable_stmt);

  //       /*
  //         Depending on the constant evaluation mark the then/else blocks unreachable.
  //         This should be done after the pior calls to reachability so the topmost
  //         unreachable statement is marked first.
  //       */
  //       if (const_expr.is_const_expr()) {
  //         bool cond_value = std::get<bool>(const_expr.const_expr_value);
  //         if (cond_value) {
  //           if (if_stmt.has_else) {
  //             LAST_UNREACHABLE_STMT(if_stmt.else_block);
  //           }
  //         } else {
  //           LAST_UNREACHABLE_STMT(if_stmt.then_block);
  //         }
  //       }

  //       return if_returns;
  //     }, if_stmt.cond->v);
  //   },
  //   pattern(as<Ast_Return_Statement>(_)) = []{ return true; },
  //   pattern(_) = []{ return false; });
}

}
