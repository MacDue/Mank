#include <cassert>
#include <type_traits>

#include <mpark/patterns.hpp>

#include "ast/ast.h"
#include "sema/types.h"
#include "sema/const_propagate.h"
#include "errors/compiler_errors.h"

namespace AstHelper {

/* Dumb stuff I don't want to see! */
#define WHEN_TYPE(runtime_check, const_check, op)  { \
    auto constexpr pass_check = const_check;         \
    if constexpr (pass_check) {                      \
      WHEN(runtime_check && (const_check)) {         \
        return PrimativeValue(op);                   \
      };                                             \
    } else {                                         \
      WHEN(false) {                                  \
        return PrimativeValue(false);                \
      };                                             \
    }                                                \
  }

#define CHECK(check, number) check<decltype(number)>
#define SAME_TYPES(a,b) std::is_same_v<decltype(a), decltype(b)>
#define LHS_RHS_MATCH() SAME_TYPES(lhs, rhs)

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

/*
  Constant expression eval!

  The idea behind these const eval functions are that it'll only eval
  if the lhs & rhs (or operand) are both constants.

  Otherwise it'll just pass through the monostate (which just means it's non-constant).
*/

void ConstantVisitor::after(Ast_Binary_Operation& binop) {
  using namespace mpark::patterns;
  auto op = binop.operation;
  auto& lhs = binop.left->meta.const_value;
  auto& rhs = binop.right->meta.const_value;
  binop.update_const_value(
    match(lhs, rhs)(
      pattern(vis(arg), vis(arg)) = [&](auto lhs, auto rhs){
      WHEN_TYPE(is_integer_op(op), LHS_RHS_MATCH() && CHECK(std::is_integral_v, lhs) && CHECK(std::is_integral_v, rhs), [&]{
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
      WHEN_TYPE(!is_comparison_op(op), LHS_RHS_MATCH() && CHECK(std::is_arithmetic_v, lhs) && CHECK(std::is_arithmetic_v, rhs), [&]{
        switch (op) {
          case Ast_Operator::PLUS:
            return lhs + rhs;
          case Ast_Operator::MINUS:
            return lhs - rhs;
          case Ast_Operator::TIMES:
            return lhs * rhs;
          case Ast_Operator::DIVIDE:
            if (rhs == 0) {
              throw_error_at(binop, "division by zero");
            }
            return lhs / rhs;
          default:
            assert(false && "fix me! unknown numeric operator in constant binary expr eval");
        }
      }());
    },
    pattern(vis(arg), vis(arg)) = [&](auto lhs, auto rhs){
      WHEN_TYPE(true, LHS_RHS_MATCH() && CHECK(std::is_arithmetic_v, lhs) && CHECK(std::is_arithmetic_v, rhs), [&]{
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
    pattern(as<std::string>(arg), as<std::string>(arg)) = [&](auto& lhs, auto& rhs) {
      assert(op == Ast_Operator::PLUS);
      return PrimativeValue(lhs + rhs);
    },
    pattern(_, _) = [&]{ return PrimativeValue(); }
  ));
}

void ConstantVisitor::after(Ast_Unary_Operation& unary) {
  using namespace mpark::patterns;
  unary.update_const_value(
    match(unary.operand->meta.const_value, unary.operation)(
      pattern(vis(arg), Ast_Operator::MINUS) = [&](auto number){
        WHEN_TYPE(true, CHECK(std::is_arithmetic_v, number), -number);
      },
      pattern(as<int>(arg), Ast_Operator::BITWISE_NOT) = [](auto integer) {
        return PrimativeValue(~integer);
      },
      pattern(as<bool>(arg), Ast_Operator::LOGICAL_NOT) = [](auto boolean) {
        return PrimativeValue(!boolean);
      },
      pattern(_, _) = [&](){ return PrimativeValue(); }));
}

void ConstantVisitor::after(Ast_Index_Access& index) {
  static_check_array_bounds(index, true);
}

void ConstantVisitor::after(Ast_Tuple_Literal& tuple_literal) {
  // Fixes lvalue tuples (not needed after semantics checking)
  tuple_literal.get_self().class_ptr()->set_value_type(Expression_Meta::RVALUE);
}

static char parse_char_literal(std::string_view literal) {
  // 'a'
  // '\a'
  if (literal[1] == '\\') {
    switch (literal[2]) {
      case 'b':
        return '\b';
      case 'n':
        return '\n';
      case 't':
        return '\t';
      case 'f':
        return '\f';
      case 'r':
        return '\r';
      case 'e':
        return '\033';
      default:
        return literal[2];
    }
  } else {
    return literal[1];
  }
}

static std::string parse_string_literal(std::string_view literal) {
  // "Hello World"
  // "Hello\nWorld";
  // "Hello \"World\""
  std::string result;
  result.reserve(literal.size() - 2);
  for (size_t idx = 1; idx < literal.length() - 1; idx += 1) {
    if (literal[idx] == '\\') {
      result += parse_char_literal(literal.substr(idx - 1, 3));
      idx += 1;
    } else {
      result += literal[idx];
    }
  }
  return result;
}

void ConstantVisitor::visit(Ast_Literal& literal) {
  auto& value = literal.value;
  switch (literal.literal_type) {
    case PrimativeType::INTEGER:
      literal.update_const_value(std::stoi(value));
      break;
    case PrimativeType::FLOAT64:
      literal.update_const_value(std::stod(value));
      break;
    case PrimativeType::FLOAT32:
      literal.update_const_value(std::stof(value));
      break;
    case PrimativeType::BOOL:
      literal.update_const_value(value == "true" ? true : false);
      break;
    case PrimativeType::STRING:
      literal.update_const_value(parse_string_literal(value));
      break;
    case PrimativeType::CHAR:
     literal.update_const_value(parse_char_literal(value));
     break;
    default:
      assert(false && "fix me! unknown primative type");
  }
}

}
