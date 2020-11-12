#pragma once

#include <cstdint>
#include <variant>

using PrimativeValue = std::variant<
  std::monostate,
  float,
  double,
  /* strings? */
  int32_t,
  uint8_t,
  bool>;

struct PrimativeType {
  enum Tag {
    FLOAT32,
    FLOAT64,
    STRING,
    INTEGER,
    UNSIGNED_BYTE,
    BOOL,
    /* TODO: more integer types */
  } tag;

  PrimativeType() = default;
  PrimativeType(Tag tag)
    : tag{tag} {}

  static char const * type_name(Tag type_tag);
  inline char const * name() const { return type_name(tag); }

  static uint type_size(Tag type_tag);
  inline uint size() const { return type_size(tag); }

  bool is_numeric_type() const;
  bool is_integer_type() const;
  bool is_float_type() const;
  bool is_string_type() const;
  bool is_boolean_type() const;
};
