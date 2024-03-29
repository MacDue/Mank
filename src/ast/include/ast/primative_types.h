#pragma once

#include <string>
#include <cstdint>
#include <variant>

#include "ast/type_var.h"
#include "ast/self_helper.h"

using PrimativeValue = std::variant<
  std::monostate,
  float,
  double,
  int32_t,
  int64_t,
  uint8_t,
  bool,
  char,
  std::string>; // it's not really primative but convenient

DEF_TYPE(PrimativeType) {
  enum Tag {
    FLOAT32,
    FLOAT64,
    STRING,
    INTEGER,
    INTEGER64,
    UNSIGNED_BYTE,
    BOOL,
    CHAR
    /* TODO: more integer types */
  } tag;

  static Type_Ptr get(Tag tag);

  inline static Type_Ptr bool_ty() { return get(BOOL); }
  inline static Type_Ptr int_ty() { return get(INTEGER); }
  inline static Type_Ptr str_ty() { return get(STRING); }
  inline static Type_Ptr f32_ty() { return get(FLOAT32); }
  inline static Type_Ptr f64_ty() { return get(FLOAT64); }
  inline static Type_Ptr char_ty() { return get(CHAR); }

  static char const * type_name(Tag type_tag);

  inline char const * name() const { return type_name(tag); }

  static uint type_size(Tag type_tag);
  inline uint size() const { return type_size(tag); }

  bool is_numeric_type() const;
  bool is_integer_type() const;
  bool is_float_type() const;
  bool is_string_type() const;
  bool is_boolean_type() const;
  bool is_char_type() const;

  bool is_signed() const;

  bool satisfies(TypeVar::Constraint constraint) const;
private:
  PrimativeType() = default;
  PrimativeType(Tag tag)
    : tag{tag} {}
};
