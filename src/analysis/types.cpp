#include <string>
#include <cassert>
#include <type_traits>
#include <mpark/patterns.hpp>
#include <formatxx/std_string.h>

#include "ast.h"
#include "types.h"

char const * literal_type_to_string(PrimativeTypeTag type) {
  switch (type) {
    case PrimativeTypeTag::FLOAT32:
      return "Float32";
    case PrimativeTypeTag::FLOAT64:
      return "Float64";
    case PrimativeTypeTag::INTEGER:
      return "Integer";
    case PrimativeTypeTag::STRING:
      return "Static string";
    case PrimativeTypeTag::BOOL:
      return "Boolean";
    default:
      return "???";
  }
}

std::string type_to_string(Type& type) {
  using namespace std::string_literals;
  using namespace mpark::patterns;
  return match(type.v)(
    pattern(as<UncheckedType>(arg)) = [](auto & unchecked_type) {
      return formatxx::format_string("unchecked type - {}", unchecked_type.identifer.name);
    },
    pattern(as<PrimativeType>(arg)) = [](auto & primative_type) {
      return std::string(literal_type_to_string(primative_type.tag));
    },
    pattern(_) = []{
      return "???"s;
    });
}

std::string type_to_string(Type* type) {
  if (type) {
    return type_to_string(*type);
  } else {
    return "Void";
  }
}

Type_Ptr extract_type(std::weak_ptr<Type> weak_type_ptr) {
  if (auto type_ptr = weak_type_ptr.lock()) {
    return type_ptr;
  }
  assert(false && "fix me! expression type imformation is missing!");
}

uint primative_size(PrimativeTypeTag type_tag) {
  switch (type_tag) {
    case PrimativeTypeTag::FLOAT32:
      return 32;
    case PrimativeTypeTag::FLOAT64:
      return 64;
    case PrimativeTypeTag::INTEGER:
      return 32;
    case PrimativeTypeTag::STRING: {
      assert(false && "depends on target");
      return 0;
    }
    case PrimativeTypeTag::BOOL:
      return 1;
    default:
      assert(false && "fix me! unknown primative size");
  }
}

bool numeric_type(PrimativeTypeTag type_tag) {
  return integer_type(type_tag) || float_type(type_tag);
}

bool integer_type(PrimativeTypeTag type_tag) {
  return type_tag == PrimativeTypeTag::INTEGER;
}

bool float_type(PrimativeTypeTag type_tag) {
  return type_tag == PrimativeTypeTag::FLOAT32 || type_tag == PrimativeTypeTag::FLOAT64;
}

bool string_type(PrimativeTypeTag type_tag) {
  return type_tag == PrimativeTypeTag::STRING;
}

bool boolean_type(PrimativeTypeTag type_tag) {
  return type_tag == PrimativeTypeTag::BOOL;
}

/* Primative parsing */

Ast_Literal::Ast_Literal(SourceLocation location,
  std::string value, PrimativeTypeTag type
)
  : Ast_Const_Expr(location), literal_type{type}, value{value}
{
  switch (type) {
  case PrimativeTypeTag::INTEGER:
    this->const_expr_value = std::stoi(value);
    break;
  case PrimativeTypeTag::FLOAT64:
    this->const_expr_value = std::stod(value);
    break;
  case PrimativeTypeTag::FLOAT32:
    this->const_expr_value = std::stof(value);
    break;
  case PrimativeTypeTag::BOOL:
    this->const_expr_value = value == "true" ? true : false;
    break;
  case PrimativeTypeTag::STRING:
    break;
  default:
    assert(false && "fix me! unknown primative type");
  }
}

int32_t Ast_Literal::as_int32() {
  assert(literal_type == PrimativeTypeTag::INTEGER);
  return std::get<int32_t>(this->const_expr_value);
}

double Ast_Literal::as_float64() {
  assert(literal_type == PrimativeTypeTag::FLOAT64);
  return std::get<double>(this->const_expr_value);
}

float Ast_Literal::as_float32() {
  assert(literal_type == PrimativeTypeTag::FLOAT32);
  return std::get<float>(this->const_expr_value);
}

bool Ast_Literal::as_bool() {
  assert(literal_type == PrimativeTypeTag::BOOL);
  return std::get<bool>(this->const_expr_value);
}

int Ast_Literal::size_bytes() {
  return primative_size(literal_type);
}

/* Const expr */

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
