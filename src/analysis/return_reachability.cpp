#include <cassert>
#include <mpark/patterns.hpp>
#include "ast/return_reachability.h"
#include "ast_printer.h"
#include <iostream>

namespace AstHelper {

/*
TODO: make not horrible
TODO: REDO THIS CODE IT'S A HORRIBLE MESSY HACK!
*/

bool is_return_block(Ast_Block& block, Ast_Return_Statement** out_return_stmt) {
  using namespace mpark::patterns;
  for (auto& stmt: block.statements) {
    bool is_return_stmt = match(stmt->v)(
      pattern(as<Ast_Return_Statement>(arg)) = [&](auto& found_return_stmt) {
        if (out_return_stmt) {
          *out_return_stmt = &found_return_stmt;
        }
        return true;
      },
      pattern(as<Ast_If_Statement>(_)) = [&](){
        /* HACK HACK HACK HACK FIX ME FIX ME */
        // This is only so it works on semantics!
        // the code generator wants to check for shallow returns only
        // while sema wants nested retursn this should be split out MUCH better
        WHEN(out_return_stmt) {
          /* Hardcoded for sema */
          return block_returns(*stmt, out_return_stmt);
        };
      },
      pattern(_) = []{ return false; });
    if (is_return_stmt) {
      return true;
    }
  }
  return false;
}

bool block_returns(Ast_Statement& block_like, Ast_Return_Statement** out_return_stmt, bool only_last) {
  /* MORE HACK HACK HACKS */
  using namespace mpark::patterns;
  return match(block_like.v)(
    pattern(as<Ast_Block>(arg)) = [&](auto& block) {
      return is_return_block(block, out_return_stmt);
    },
    pattern(as<Ast_If_Statement>(arg)) = [&](auto& if_stmt) {
      if (only_last) {
        if (!if_stmt.has_else) {
          return is_return_block(
            std::get<Ast_Block>(if_stmt.then_block->v), out_return_stmt);
        } else {
          return block_returns(*if_stmt.else_block, out_return_stmt, only_last);
        }
      } else {
        bool then_returns = is_return_block(
          std::get<Ast_Block>(if_stmt.then_block->v), out_return_stmt);
        if (!if_stmt.has_else) {
          return then_returns;
        } else {
          return then_returns && block_returns(*if_stmt.else_block, out_return_stmt, only_last);
        }
      }
    },
    pattern(_) = []{
      // assert(false && "not block like statement!");
      return false;
    }
  );
}

}
