#include <cassert>

#include <mpark/patterns.hpp>
#include <formatxx/std_string.h>

#include "ast/ast.h"
#include "ast/types.h"
#include "ast/context.h"

/* String helpers */

static std::string type_list_to_string(
  std::vector<Type_Ptr> const & types,
  std::string_view seperator, std::string_view begin_padding = ""
) {
  std::string out;
  for (
    auto it = types.begin(); it != types.end(); it++
  ) {
    if (it != types.begin()) {
      out += seperator;
    } else {
      out += begin_padding;
    }
    out += type_to_string(*it->get());
  }
  return out;
}

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

      lambda_str += type_list_to_string(lambda_type.argument_types, ", ", " ");
      lambda_str += " -> " + type_to_string(lambda_type.return_type.get());
      return lambda_str;
    },
    pattern(as<TupleType>(arg)) = [](auto const & tuple_type) {
      return formatxx::format_string("({})",
        type_list_to_string(tuple_type.element_types, ","));
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
    pattern(as<TypeFieldConstraint>(arg)) = [](auto const & field_constraint) {
      return formatxx::format_string("{}[.{}]",
        type_to_string(field_constraint.type.get()),
        field_constraint.field_access->field.name);
    },
    pattern(_) = []{
      return "???"s;
    });
}

std::string type_to_string(Type const * type) {
  if (type) {
    return type_to_string(*type);
  } else {
    return "[MISSING TYPE]";
  }
}

/* Type */

Type_Ptr Type::void_ty() {
  static Type void_type{TupleType()};
  return AstContext::make_static_type_ptr(&void_type);
}

bool Type::is_void() const {
  if (auto tuple_type = std::get_if<TupleType>(&v)) {
    return tuple_type->element_types.size() == 0;
  }
  return false;
}

/* Type Var */

Type_Ptr TypeVar::get(Constraint constraint) {
  switch (constraint) {
    case NUMERIC: {
      static Type numeric{TypeVar(NUMERIC)};
      return AstContext::make_static_type_ptr(&numeric);
    }
    case INTEGER: {
      static Type integer{TypeVar(INTEGER)};
      return AstContext::make_static_type_ptr(&integer);
    }
    default:
      assert(false && "fix me! unknown constraint");
  }
}

Type_Ptr TypeFieldConstraint::get(AstContext& ctx, Ast_Field_Access& access) {
  TypeFieldConstraint fc;
  fc.type = access.object->meta.type;
  fc.field_access = &access;
  return ctx.new_type(fc);
}
