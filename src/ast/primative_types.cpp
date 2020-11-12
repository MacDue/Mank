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
    case PrimativeType::UNSIGNED_BYTE:
      return "Unsigned byte";
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
    case PrimativeType::UNSIGNED_BYTE:
      return 8;
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
  return tag == PrimativeType::INTEGER || tag == PrimativeType::UNSIGNED_BYTE;
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

int32_t Ast_Literal::as_int32() {
  assert(literal_type == PrimativeType::INTEGER);
  return std::get<int32_t>(this->const_value());
}

double Ast_Literal::as_float64() {
  assert(literal_type == PrimativeType::FLOAT64);
  return std::get<double>(this->const_value());
}

float Ast_Literal::as_float32() {
  assert(literal_type == PrimativeType::FLOAT32);
  return std::get<float>(this->const_value());
}

bool Ast_Literal::as_bool() {
  assert(literal_type == PrimativeType::BOOL);
  return std::get<bool>(this->const_value());
}

int Ast_Literal::size_bytes() {
  return PrimativeType::type_size(literal_type);
}
