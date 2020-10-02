#pragma once

enum class PrimativeTypeTag {
  FLOAT32,
  FLOAT64,
  STRING,
  INTEGER,
  BOOL,
  /* TODO: more integer types */
};

bool numeric_type(PrimativeTypeTag type_tag);
bool integer_type(PrimativeTypeTag type_tag);
bool float_type(PrimativeTypeTag type_tag);
bool string_type(PrimativeTypeTag type_tag);
bool boolean_type(PrimativeTypeTag type_tag);
