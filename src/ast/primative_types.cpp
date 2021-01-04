#include <cassert>

#include "ast/expr.h"
#include "ast/types.h"
#include "ast/primative_types.h"

Type_Ptr PrimativeType::get(PrimativeType::Tag tag) {
  switch (tag) {
    case FLOAT32: {
      static Type f32{PrimativeType(FLOAT32)};
      return AstContext::make_static_type_ptr(&f32);
    }
    case FLOAT64: {
      static Type f64{PrimativeType(FLOAT64)};
      return AstContext::make_static_type_ptr(&f64);
    }
    case INTEGER: {
      static Type i32{PrimativeType(INTEGER)};
      return AstContext::make_static_type_ptr(&i32);
    }
    case UNSIGNED_BYTE: {
      static Type u8{PrimativeType(UNSIGNED_BYTE)};
      return AstContext::make_static_type_ptr(&u8);
    }
    case STRING: {
      static Type str{PrimativeType(STRING)};
      return AstContext::make_static_type_ptr(&str);
    }
    case BOOL: {
      static Type Bool{PrimativeType(BOOL)};
      return AstContext::make_static_type_ptr(&Bool);
    }
    case CHAR: {
      static Type Char{PrimativeType(CHAR)};
      return AstContext::make_static_type_ptr(&Char);
    }
    default:
      assert(false && "fix me! missing primative!");
      return nullptr;
  }
}

char const * PrimativeType::type_name(Tag type) {
  switch (type) {
    case PrimativeType::FLOAT32:
      return "Float32";
    case PrimativeType::FLOAT64:
      return "Float64";
    case PrimativeType::INTEGER:
      return "Integer32";
    case PrimativeType::UNSIGNED_BYTE:
      return "Unsigned byte";
    case PrimativeType::STRING:
      return "String";
    case PrimativeType::BOOL:
      return "Boolean";
    case PrimativeType::CHAR:
      return "Char";
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
    case PrimativeType::CHAR:
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
  return tag == PrimativeType::INTEGER
    || tag == PrimativeType::UNSIGNED_BYTE
    || tag == PrimativeType::CHAR; // _pretty_ much ;)
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

bool PrimativeType::is_char_type() const {
  return tag == PrimativeType::CHAR;
}

bool PrimativeType::is_signed() const {
  return !is_boolean_type();
}


bool PrimativeType::satisfies(TypeVar::Constraint constraint) const {
  switch (constraint) {
    case TypeVar::Constraint::NUMERIC:
      return is_numeric_type();
    case TypeVar::Constraint::INTEGER:
      return is_integer_type();
    case TypeVar::Constraint::ADDABLE:
      return is_numeric_type() || is_string_type();
    default:
      assert(false && "fix me! unknown constraint");
  }
}

/* AST Literal */

int32_t Ast_Literal::as_int32() const  {
  assert(literal_type == PrimativeType::INTEGER);
  return std::get<int32_t>(this->const_value());
}

double Ast_Literal::as_float64() const {
  assert(literal_type == PrimativeType::FLOAT64);
  return std::get<double>(this->const_value());
}

float Ast_Literal::as_float32() const {
  assert(literal_type == PrimativeType::FLOAT32);
  return std::get<float>(this->const_value());
}

bool Ast_Literal::as_bool() const {
  assert(literal_type == PrimativeType::BOOL);
  return std::get<bool>(this->const_value());
}

char Ast_Literal::as_char() const {
  assert(literal_type == PrimativeType::CHAR);
  return std::get<char>(this->const_value());
}

std::string Ast_Literal::as_string() const {
  assert(literal_type == PrimativeType::STRING);
  return std::get<std::string>(this->const_value());
}


int Ast_Literal::size_bytes() {
  return PrimativeType::type_size(literal_type);
}
