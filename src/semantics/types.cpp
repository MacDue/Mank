#include <mpark/patterns.hpp>

#include "sema/types.h"

static bool match_type_lists(
  std::vector<Type_Ptr> const & a,
  std::vector<Type_Ptr> const & b
) {
  if (a.size() != b.size()) {
    return false;
  }
  for (size_t idx = 0; idx < a.size(); idx++) {
    if (!match_types(a.at(idx).get(), b.at(idx).get())) {
      return false;
    }
  }
  return true;
}

bool match_types(Type const * a, Type const * b) {
  using namespace mpark::patterns;

  // Reference types should be matched like normal versions of their type
  // only when binding them should their be special treatment
  a = remove_reference(a);
  b = remove_reference(b);

  if (a == b) {
    return true;
  } else if (a && b) {
    return match(a->v, b->v)(
      pattern(as<PrimativeType>(arg), as<PrimativeType>(arg)) =
        [](auto const & a, auto const & b) {
          return a.tag == b.tag;
        },
      pattern(as<FixedSizeArrayType>(arg), as<FixedSizeArrayType>(arg)) =
        [](auto const & a, auto const & b) {
          return a.size == b.size
            && match_types(a.element_type.get(), b.element_type.get());
        },
      pattern(as<LambdaType>(arg), as<LambdaType>(arg)) =
        [](auto const & a, auto const & b) {
          return match_type_lists(a.argument_types, b.argument_types)
            && match_types(a.return_type.get(), b.return_type.get());
        },
      pattern(as<TypeVar>(arg), as<TypeVar>(arg)) =
        [](auto const & a, auto const & b) {
          return a.id == b.id;
        },
      pattern(_, _) = []{ return false; });
  }
  return false;
}

TypeResolution resolve_type(Scope& scope, Type_Ptr type) {
  using namespace mpark::patterns;
  static auto null_symbol = std::optional<Ast_Identifier>{};

  return match(type->v)(
    pattern(as<UncheckedType>(arg)) = [&](auto const & unchecked) {
      Symbol* symbol = scope.lookup_first_name(unchecked.identifier);
      auto resolved_type = symbol && symbol->kind == Symbol::TYPE
        ? symbol->type : nullptr;
      return std::make_pair(resolved_type, std::optional{unchecked.identifier});
    },
    pattern(as<FixedSizeArrayType>(arg)) = [&](auto& array_type) {
      auto [element_type, symbol] = resolve_type(scope, array_type.element_type);
      array_type.element_type = element_type;
      return std::make_pair(element_type ? type : nullptr, symbol);
    },
    pattern(as<ReferenceType>(arg)) = [&](auto& reference_type) {
      auto [referenced_type, symbol] = resolve_type(scope, reference_type.references);
      reference_type.references = referenced_type;
      return std::make_pair(referenced_type ? type : nullptr, symbol);
    },
    pattern(as<LambdaType>(arg)) = [&](auto& lambda_type) {
      if (lambda_type.return_type) { // hack solution to returing nothing
        auto [return_type, return_symbol] = resolve_type(scope, lambda_type.return_type);
        lambda_type.return_type = return_type;

        if (!return_type) {
          return std::make_pair(Type_Ptr(nullptr), return_symbol);
        }
      }

      for (auto& arg_type: lambda_type.argument_types) {
        auto [resolved_arg_type, arg_symbol] = resolve_type(scope, arg_type);
        arg_type = resolved_arg_type;
        if (!arg_type) {
          return std::make_pair(Type_Ptr(nullptr), arg_symbol);
        }
      }

      return std::make_pair(type, null_symbol);
    },
    pattern(_) = [&] {
      // Possible it's already resolved
      // TODO: Make sure code does not recurse over already resolved types.
      return std::make_pair(type, null_symbol);
    });
}
