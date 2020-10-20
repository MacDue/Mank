#include <cassert>
#include <mpark/patterns.hpp>

#include "sema/return_reachability.h"

namespace AstHelper {

#define LAST_UNREACHABLE_STMT(stmt)   \
  if (unreachable_stmt != nullptr) {  \
      *unreachable_stmt = &*(stmt);   \
  }

static inline Statement_Ptr first_statement_in_block(Ast_Block& block) {
  if (block.statements.size() > 0) {
    return block.statements.at(0);
  }
  return nullptr;
}

static inline Statement_Ptr first_statement_in_block(Expression_Ptr& expr) {
  return first_statement_in_block(std::get<Ast_Block>(expr->v));
}

bool check_reachability(Ast_Block& block, Ast_Statement** unreachable_stmt) {
  for (uint stmt_idx = 0; stmt_idx < block.statements.size(); stmt_idx++) {
    auto& stmt = block.statements.at(stmt_idx);
    if (check_reachability(*stmt, unreachable_stmt)) {
      if (stmt_idx + 1 < block.statements.size()) {
        LAST_UNREACHABLE_STMT(block.statements.at(stmt_idx + 1));
      }
      return true;
    }
  }
  return false;
}

bool check_reachability(Ast_Statement& statement, Ast_Statement** unreachable_stmt) {
  using namespace mpark::patterns;
  return match(statement.v)(
    pattern(as<Ast_Return_Statement>(_)) = []{ return true; },
    pattern(anyof(as<Ast_Expression_Statement>(arg), as<Ast_Assign>(arg))) = [&](auto& expr_stmt){
      return check_reachability(*expr_stmt.expression, unreachable_stmt);
    },
    pattern(as<Ast_Variable_Declaration>(arg)) = [&](auto var_decl){
      if (var_decl.initializer) {
        return check_reachability(*var_decl.initializer, unreachable_stmt);
      }
      return false;
    },
    pattern(as<Ast_For_Loop>(arg)) = [&](auto& for_loop){
      bool start_range_returns = check_reachability(*for_loop.start_range, unreachable_stmt);
      bool end_range_returns = check_reachability(*for_loop.end_range, unreachable_stmt);
      bool body_returns = check_reachability(for_loop.body, unreachable_stmt);

      if (start_range_returns || end_range_returns) {
        LAST_UNREACHABLE_STMT(first_statement_in_block(for_loop.body));
      }

      return start_range_returns || end_range_returns || body_returns;
    },
    pattern(_) = []{
      assert(false && "fix me! unknown statement in reachability checking");
      return false;
    }
  );
}

bool check_reachability(Ast_Expression& block_like, Ast_Statement** unreachable_stmt) {
  using namespace mpark::patterns;
  return match(block_like.v)(
    pattern(as<Ast_Block>(arg)) = [&](auto& block) {
      return check_reachability(block, unreachable_stmt);
    },
    pattern(as<Ast_If_Expr>(arg)) = [&](auto& if_stmt) {
      /*
        When deciding if a if statement returns the constant evaluation should not
        be used. As it can make it confusing, as when the constant eval improves,
        previously rejected functions can be accepted.

        Things can also get arbitrarily complex.
        e.g.

        fun fermats_last_theorm: bool (x: i32, y: i32, z: i32, n: i32) {
          if n > 2 {
            if x^n + y^n == z^n {
              # A sufficiently advanced compiler could prove this is
              # unreachable and hence not matter there's no return.
            }
            return false;
          } else {
            return true;
          }
        }
      */

      // Do these separately as we still want warnings in the else block even if the
      // then block does not return.
      bool then_returns = check_reachability(*if_stmt.then_block, unreachable_stmt);
      bool else_returns = if_stmt.has_else && check_reachability(*if_stmt.else_block, unreachable_stmt);

      /*
        Depending on the constant evaluation mark the then/else blocks unreachable.
        This should be done after the pior calls to reachability so the topmost
        unreachable statement is marked first.
      */
      auto& const_expr = variant_cast<Ast_Const_Expr>(if_stmt.cond->v);
      if (const_expr.is_const_expr()) {
        bool cond_value = std::get<bool>(const_expr.const_expr_value);
        if (cond_value) {
          if (if_stmt.has_else) {
            LAST_UNREACHABLE_STMT(first_statement_in_block(if_stmt.else_block));
          }
        } else {
          LAST_UNREACHABLE_STMT(first_statement_in_block(if_stmt.then_block));
        }
      }

      // If can only return on _all_ paths if it has an else block, which also returns.
      return then_returns && else_returns;
    },
    pattern(as<Ast_Call>(arg)) = [&](auto& call) {
      for (auto& arg: call.arguments) {
        if (check_reachability(*arg, unreachable_stmt)) {
          return true;
        }
      }
      return false;
    },
    pattern(as<Ast_Unary_Operation>(arg)) = [&](auto& unary) {
      return check_reachability(*unary.operand, unreachable_stmt);
    },
    pattern(as<Ast_Binary_Operation>(arg)) = [&](auto binary) {
      return check_reachability(*binary.left, unreachable_stmt)
        || check_reachability(*binary.right, unreachable_stmt);
    },
    pattern(anyof(as<Ast_Literal>(_), as<Ast_Identifier>(_))) = []{
      return false;
    },
    pattern(_) = []{
      assert(false && "fix me! unknown expression in reachability checking");
      return false;
    });
}

}
