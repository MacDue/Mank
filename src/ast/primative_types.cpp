#include <cassert>

#include "ast/expr.h"
#include "ast/primative_types.h"

char const * PrimativeType::type_name(Tag type) {
  switch (type) {
    case PrimativeType::FLOAT32:
      return "Float32";
    case PrimativeType::FLOAT64:
      return "Float64";
    case PrimativeType::INTEGER:
      return "Integer";
    case PrimativeType::STRING:
      return "Static string";
    case PrimativeType::BOOL:
      return "Boolean";
    default:
      return "???";
  }
}

uint PrimativeType::type_size(Tag type_tag) {
  switch (type_tag) {
    case PrimativeType::FLOAT32:
      return 32;
    case PrimativeType::FLOAT64:
      return 64;
    case PrimativeType::INTEGER:
      return 32;
    case PrimativeType::STRING: {
      assert(false && "depends on target");
      return 0;
    }
    case PrimativeType::BOOL:
      return 1;
    default:
      assert(false && "fix me! unknown primative size");
  }
}

bool PrimativeType::is_numeric_type() const {
  return is_integer_type() || is_float_type();
}

bool PrimativeType::is_integer_type() const {
  return tag == PrimativeType::INTEGER;
}

bool PrimativeType::is_float_type() const {
  return tag == PrimativeType::FLOAT32 || tag == PrimativeType::FLOAT64;
}

bool PrimativeType::is_string_type() const {
  return tag == PrimativeType::STRING;
}

bool PrimativeType::is_boolean_type() const {
  return tag == PrimativeType::BOOL;
}

/* AST Literal */

Ast_Literal::Ast_Literal(SourceLocation location,
  std::string value, PrimativeType::Tag type
): Ast_Const_Expr(location), literal_type{type}, value{value}
{
  switch (type) {
  case PrimativeType::INTEGER:
    this->const_expr_value = std::stoi(value);
    break;
  case PrimativeType::FLOAT64:
    this->const_expr_value = std::stod(value);
    break;
  case PrimativeType::FLOAT32:
    this->const_expr_value = std::stof(value);
    break;
  case PrimativeType::BOOL:
    this->const_expr_value = value == "true" ? true : false;
    break;
  case PrimativeType::STRING:
    break;
  default:
    assert(false && "fix me! unknown primative type");
  }
}

int32_t Ast_Literal::as_int32() {
  assert(literal_type == PrimativeType::INTEGER);
  return std::get<int32_t>(this->const_expr_value);
}

double Ast_Literal::as_float64() {
  assert(literal_type == PrimativeType::FLOAT64);
  return std::get<double>(this->const_expr_value);
}

float Ast_Literal::as_float32() {
  assert(literal_type == PrimativeType::FLOAT32);
  return std::get<float>(this->const_expr_value);
}

bool Ast_Literal::as_bool() {
  assert(literal_type == PrimativeType::BOOL);
  return std::get<bool>(this->const_expr_value);
}

int Ast_Literal::size_bytes() {
  return PrimativeType::type_size(literal_type);
}
