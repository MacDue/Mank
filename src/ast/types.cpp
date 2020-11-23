#include <cassert>

#include <mpark/patterns.hpp>
#include <formatxx/std_string.h>

#include "ast/types.h"
#include "ast/ast_builder.h"

/* String helpers */

std::string type_to_string(Type const & type) {
  using namespace std::string_literals;
  using namespace mpark::patterns;
  return match(type.v)(
    pattern(as<UncheckedType>(arg)) = [](auto const & unchecked_type) {
      return formatxx::format_string("unchecked type - {}", unchecked_type.identifier.name);
    },
    pattern(as<PrimativeType>(arg)) = [](auto const & primative_type) {
      return std::string(primative_type.name());
    },
    pattern(as<Ast_Pod_Declaration>(arg)) = [](auto const & pod_type) {
      return formatxx::format_string("pod {}", pod_type.identifier.name);
    },
    pattern(as<FixedSizeArrayType>(arg)) = [](auto const & array_type) {
      return formatxx::format_string("{}[{}]",
        type_to_string(array_type.element_type.get()), array_type.size);
    },
    pattern(as<ReferenceType>(arg)) = [](auto const & reference_type) {
      return formatxx::format_string("reference to {}",
        type_to_string(reference_type.references.get()));
    },
    pattern(as<LambdaType>(arg)) = [](auto const & lambda_type) {
      std::string lambda_str = "lambda";
      for (
        auto it = lambda_type.argument_types.begin();
        it != lambda_type.argument_types.end();
        it++
      ) {
        if (it != lambda_type.argument_types.begin()) {
          lambda_str += ", ";
        } else {
          lambda_str += ' ';
        }
        lambda_str += type_to_string(it->get());
      }
      lambda_str += " -> " + type_to_string(lambda_type.return_type.get());
      return lambda_str;
    },
    pattern(as<TypeVar>(arg)) = [](auto const & type_var) {
      if (type_var.special()) {
        switch (type_var.id)
        {
          case TypeVar::INTEGER:
            return "IntegerType"s;
          case TypeVar::NUMERIC:
            return "NumericType"s;
          default:
            return "???"s;
        }
      }
      return formatxx::format_string("T{}", type_var.id);
    },
    pattern(_) = []{
      return "???"s;
    });
}

std::string type_to_string(Type const * type) {
  if (type && !std::holds_alternative<VoidType>(type->v)) {
    return type_to_string(*type);
  } else {
    return "Void";
  }
}

/* Helpers */

Type_Ptr extract_type_nullable(std::weak_ptr<Type> weak_type_ptr) {
  if (auto type_ptr = weak_type_ptr.lock()) {
    return type_ptr;
  }
  return nullptr;
}

Type_Ptr extract_type(std::weak_ptr<Type> weak_type_ptr) {
  if (auto type_ptr = extract_type_nullable(weak_type_ptr)) {
    return type_ptr;
  }
  assert(false && "fix me! expression type imformation is missing!");
}

/* Type Var */

Type_Ptr TypeVar::integer() {
  static Type_Ptr _integer = to_type_ptr(TypeVar(INTEGER));
  return _integer;
}

Type_Ptr TypeVar::numeric() {
  static Type_Ptr _numeric = to_type_ptr(TypeVar(NUMERIC));
  return _numeric;
}

Type_Ptr TypeVar::get(Constraint constraint) {
  switch (constraint) {
    case NUMERIC: return numeric();
    case INTEGER: return integer();
    default:
      assert(false && "fix me! unknown constraint");
  }
}
