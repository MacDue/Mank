#include <mpark/patterns.hpp>

#include "sema/types.h"

bool match_types(Type const * a, Type const * b) {
  using namespace mpark::patterns;
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
      pattern(_, _) = []{ return false; });
  }
  return false;
}

TypeResolution resolve_type(Scope& scope, Type_Ptr type) {
  using namespace mpark::patterns;
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
      return std::make_pair(type, symbol);
    },
    pattern(_) = [&] {
      return std::make_pair(Type_Ptr(nullptr), std::optional<Ast_Identifier>{});
    });
}
