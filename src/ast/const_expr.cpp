#include <cassert>
#include <type_traits>

#include <mpark/patterns.hpp>
#include <formatxx/std_string.h>

#include "ast/ast.h"

/* Dumb stuff I don't want to see! */
#define WHEN_TYPE(runtime_check, const_check, op)  { \
    auto constexpr pass_check = const_check;         \
    if constexpr (pass_check) {                      \
      WHEN(runtime_check && const_check) {           \
        return PrimativeValue(op);                   \
      };                                             \
    } else {                                         \
      WHEN(false) {                                  \
        return const_expr_value;                     \
      };                                             \
    }                                                \
  }

#define CHECK(check, number) check<decltype(number)>

/*
  Constant expression eval!

  The idea behind these const eval functions are that it'll only eval
  if the lhs & rhs (or operand) are both constants.

  Otherwise it'll just pass through the monostate (which just means it's non-constant).

  I think this is a monad lol
*/

bool Ast_Const_Expr::is_zero() {
  return std::visit([](auto value) {
    if constexpr (std::is_arithmetic_v<decltype(value)>) {
       return value == 0;
    }
    return false;
  }, const_expr_value);
}

PrimativeValue Ast_Const_Expr::const_eval_unary(Ast_Operator op) {
  using namespace mpark::patterns;
  return match(const_expr_value, op)(
    pattern(vis(arg), Ast_Operator::MINUS) = [&](auto number){
      WHEN_TYPE(true, CHECK(std::is_arithmetic_v, number), -number);
    },
    pattern(as<int>(arg), Ast_Operator::BITWISE_NOT) = [](auto integer) {
      return PrimativeValue(~integer);
    },
    pattern(as<bool>(arg), Ast_Operator::LOGICAL_NOT) = [](auto boolean) {
      return PrimativeValue(!boolean);
    },
    pattern(_, _) = [&](){ return PrimativeValue(); });
}

static bool is_comparison_op(Ast_Operator op) {
  switch (op) {
    case Ast_Operator::LESS_THAN:
    case Ast_Operator::GREATER_THAN:
    case Ast_Operator::LESS_EQUAL:
    case Ast_Operator::GREATER_EQUAL:
    case Ast_Operator::EQUAL_TO:
    case Ast_Operator::NOT_EQUAL_TO:
      return true;
    default:
      return false;
  }
}

static bool is_integer_op(Ast_Operator op) {
  switch (op) {
    case Ast_Operator::MODULO:
    case Ast_Operator::BITWISE_AND:
    case Ast_Operator::BITWISE_OR:
    case Ast_Operator::BITWISE_XOR:
    case Ast_Operator::LEFT_SHIFT:
    case Ast_Operator::RIGHT_SHIFT:
      return true;
    default:
      return false;
  }
}

PrimativeValue Ast_Const_Expr::const_eval_binary(Ast_Operator op, Ast_Const_Expr& rhs_expr) {
  using namespace mpark::patterns;
  auto& lhs = const_expr_value;
  auto& rhs = rhs_expr.const_expr_value;
  return match(lhs, rhs)(
      pattern(vis(arg), vis(arg)) = [&](auto lhs, auto rhs){
      WHEN_TYPE(is_integer_op(op), CHECK(std::is_integral_v, lhs) && CHECK(std::is_integral_v, rhs), [&]{
        switch (op) {
          case Ast_Operator::MODULO:
            return lhs % rhs;
          case Ast_Operator::BITWISE_AND:
            return lhs & rhs;
          case Ast_Operator::BITWISE_OR:
            return lhs | rhs;
          case Ast_Operator::BITWISE_XOR:
            return lhs ^ rhs;
          case Ast_Operator::LEFT_SHIFT:
            return lhs << rhs;
          case Ast_Operator::RIGHT_SHIFT:
            return lhs >> rhs;
          default:
            assert(false && "fix me! unknown integer operator in constant binary expr eval");
        }
      }());
    },
    pattern(vis(arg), vis(arg)) = [&](auto lhs, auto rhs){
      WHEN_TYPE(!is_comparison_op(op), CHECK(std::is_arithmetic_v, lhs) && CHECK(std::is_arithmetic_v, rhs), [&]{
        switch (op) {
          case Ast_Operator::PLUS:
            return lhs + rhs;
          case Ast_Operator::MINUS:
            return lhs - rhs;
          case Ast_Operator::TIMES:
            return lhs * rhs;
          case Ast_Operator::DIVIDE:
            return lhs / rhs;
          default:
            assert(false && "fix me! unknown numeric operator in constant binary expr eval");
        }
      }());
    },
    pattern(vis(arg), vis(arg)) = [&](auto lhs, auto rhs){
      WHEN_TYPE(true, CHECK(std::is_arithmetic_v, lhs) && CHECK(std::is_arithmetic_v, rhs), [&]{
        switch (op) {
          case Ast_Operator::LESS_THAN:
            return lhs < rhs;
          case Ast_Operator::GREATER_THAN:
            return lhs > rhs;
          case Ast_Operator::LESS_EQUAL:
            return lhs <= rhs;
          case Ast_Operator::GREATER_EQUAL:
            return lhs >= rhs;
          case Ast_Operator::EQUAL_TO:
            return lhs == rhs;
          case Ast_Operator::NOT_EQUAL_TO:
            return lhs != rhs;
          default:
            assert(false && "fix me! unknown comparison in constant binary expr eval");
        }
      }());
    },
    pattern(_, _) = [&]{ return PrimativeValue(); }
  );
}
