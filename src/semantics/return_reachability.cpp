#include <cassert>
#include <mpark/patterns.hpp>

#include "sema/return_reachability.h"

namespace AstHelper {

#define LAST_UNREACHABLE_STMT(stmt)           \
  if (unreachable_stmt != nullptr && stmt) {  \
      *unreachable_stmt = &*(stmt);           \
  }

static inline Stmt_Ptr first_statement_in_block(Ast_Block& block) {
  if (block.statements.size() > 0) {
    return block.statements.at(0);
  }
  return nullptr;
}

static inline Stmt_Ptr first_statement_in_block(Expr_Ptr& expr) {
  return first_statement_in_block(std::get<Ast_Block>(expr->v));
}

bool all_paths_return(Ast_Block& block, Ast_Statement** unreachable_stmt) {
  for (uint stmt_idx = 0; stmt_idx < block.statements.size(); stmt_idx++) {
    auto& stmt = block.statements.at(stmt_idx);
    bool loop_ctl = false;
    if ((loop_ctl = std::holds_alternative<Ast_Loop_Control>(stmt->v))
      || all_paths_return(*stmt, unreachable_stmt)
    ) {
      if (stmt_idx + 1 < block.statements.size()) {
        LAST_UNREACHABLE_STMT(block.statements.at(stmt_idx + 1));
      }
      return !loop_ctl;
    }
  }
  return false;
}

bool all_paths_return(Ast_Statement& statement, Ast_Statement** unreachable_stmt) {
  using namespace mpark::patterns;
  return match(statement.v)(
    pattern(as<Ast_Return_Statement>(_)) = []{ return true; },
    pattern(anyof(as<Ast_Expression_Statement>(arg), as<Ast_Assign>(arg))) = [&](auto& expr_stmt){
      return all_paths_return(*expr_stmt.expression, unreachable_stmt);
    },
    pattern(as<Ast_Variable_Declaration>(arg)) = [&](auto var_decl){
      if (var_decl.initializer) {
        return all_paths_return(*var_decl.initializer, unreachable_stmt);
      }
      return false;
    },
    pattern(as<Ast_For_Loop>(arg)) = [&](auto& for_loop){
      bool start_range_returns = all_paths_return(*for_loop.start_range, unreachable_stmt);
      bool end_range_returns = all_paths_return(*for_loop.end_range, unreachable_stmt);
      (void) all_paths_return(for_loop.body, unreachable_stmt);

      if (start_range_returns || end_range_returns) {
        LAST_UNREACHABLE_STMT(first_statement_in_block(for_loop.body));
      }

      // only 100% sure of return in range
      return start_range_returns || end_range_returns;
    },
    pattern(as<Ast_Structural_Binding>(arg)) = [&](auto& sad_binding) {
      return all_paths_return(*sad_binding.initializer, unreachable_stmt);
    },
    pattern(as<Ast_Loop>(arg)) = [&](auto& loop) {
      if (!loop.may_break) {
        return true; // it must be an infinite loop or contain a return
      }
      (void) all_paths_return(loop.body, unreachable_stmt);
      return false; // can't statically tell if the loop with end
    },
    pattern(as<Ast_While_Loop>(arg)) = [&](auto& while_loop) {
      bool return_in_cond = all_paths_return(*while_loop.cond, unreachable_stmt);
      (void) all_paths_return(while_loop.body, unreachable_stmt);

      if (return_in_cond) {
        LAST_UNREACHABLE_STMT(first_statement_in_block(while_loop.body));
      }
      return return_in_cond; // only 100% sure of return in cond
    },
    pattern(as<Ast_Loop_Control>(_)) = []{ return false; },
    pattern(_) = []{
      assert(false && "fix me! unknown statement in reachability checking");
      return false;
    }
  );
}

bool all_paths_return(Ast_Expression& block_like, Ast_Statement** unreachable_stmt) {
  using namespace mpark::patterns;
  return match(block_like.v)(
    pattern(as<Ast_Block>(arg)) = [&](auto& block) {
      return all_paths_return(block, unreachable_stmt);
    },
    pattern(as<Ast_If_Expr>(arg)) = [&](auto& if_expr) {
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
      bool then_returns = all_paths_return(*if_expr.then_block, unreachable_stmt);
      bool else_returns = if_expr.has_else && all_paths_return(*if_expr.else_block, unreachable_stmt);

      /*
        Depending on the constant evaluation mark the then/else blocks unreachable.
        This should be done after the pior calls to reachability so the topmost
        unreachable statement is marked first.
      */
      if (auto const_value = if_expr.cond->meta.get_const_value()) {
        bool cond_value = std::get<bool>(*const_value);
        if (cond_value) {
          if (if_expr.has_else) {
            LAST_UNREACHABLE_STMT(first_statement_in_block(if_expr.else_block));
          }
        } else {
          LAST_UNREACHABLE_STMT(first_statement_in_block(if_expr.then_block));
        }
      }

      // If can only return on _all_ paths if it has an else block, which also returns.
      return then_returns && else_returns;
    },
    pattern(as<Ast_Call>(arg)) = [&](auto& call) {
      return std::any_of(call.arguments.begin(), call.arguments.end(),
        [&](auto& arg) { return all_paths_return(*arg, unreachable_stmt); });
    },
    pattern(as<Ast_Unary_Operation>(arg)) = [&](auto& unary) {
      return all_paths_return(*unary.operand, unreachable_stmt);
    },
    pattern(as<Ast_Binary_Operation>(arg)) = [&](auto binary) {
      return all_paths_return(*binary.left, unreachable_stmt)
        || all_paths_return(*binary.right, unreachable_stmt);
    },
    pattern(anyof(as<Ast_Literal>(_), as<Ast_Identifier>(_),
      as<Ast_Lambda>(_), as<Ast_Path>(_))
    ) = []{
      return false;
    },
    pattern(as<Ast_Field_Access>(arg)) = [&](auto& access){
      return all_paths_return(*access.object, unreachable_stmt);
    },
    pattern(anyof(as<Ast_Array_Literal>(arg), as<Ast_Tuple_Literal>(arg))) = [&](auto& aggregate) {
      return std::any_of(aggregate.elements.begin(), aggregate.elements.end(),
        [&](auto& el){ return all_paths_return(*el, unreachable_stmt); });
    },
    pattern(as<Ast_Index_Access>(arg)) = [&](auto& index) {
      return all_paths_return(*index.object, unreachable_stmt)
        || all_paths_return(*index.index, unreachable_stmt);
    },
    pattern(as<Ast_Pod_Literal>(arg)) = [&](auto& pod) {
      return std::any_of(pod.fields.begin(), pod.fields.end(),
        [&](auto& field){ return all_paths_return(*field.initializer, unreachable_stmt); });
    },
    pattern(as<Ast_As_Cast>(arg)) = [&](auto& as_cast) {
      return all_paths_return(*as_cast.object, unreachable_stmt);
    },
    pattern(as<Ast_Array_Repeat>(arg)) = [&](auto& array_repeat) {
      return all_paths_return(*array_repeat.initializer, unreachable_stmt)
        || all_paths_return(*array_repeat.repetitions, unreachable_stmt);
    },
    pattern(as<Ast_Spawn>(arg)) = [&](auto& spawn) {
      return all_paths_return(*spawn.initializer, unreachable_stmt);
    },
    pattern(as<Ast_Switch_Expr>(arg)) = [&](auto& switch_expr){
      if (switch_expr.exhaustive) {
        return all_paths_return(*switch_expr.switched, unreachable_stmt)
          || std::all_of(switch_expr.cases.begin(), switch_expr.cases.end(),
              [&](auto& switch_case){ return all_paths_return(switch_case.body, unreachable_stmt); });
      }
      return false;
    },
    pattern(_) = []{
      assert(false && "fix me! unknown expression in reachability checking");
      return false;
    });
}

}
