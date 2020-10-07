#pragma once

#include <variant>

enum class PrimativeTypeTag {
  FLOAT32,
  FLOAT64,
  STRING,
  INTEGER,
  BOOL,
  /* TODO: more integer types */
};

using PrimativeValue = std::variant<
  std::monostate,
  float,
  double,
  /* strings? */
  int32_t,
  bool>;

uint primative_size(PrimativeTypeTag type_tag);

bool numeric_type(PrimativeTypeTag type_tag);
bool integer_type(PrimativeTypeTag type_tag);
bool float_type(PrimativeTypeTag type_tag);
bool string_type(PrimativeTypeTag type_tag);
bool boolean_type(PrimativeTypeTag type_tag);
