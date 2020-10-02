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
