#include <mpark/patterns.hpp>

#include "sema/types.h"

Type const * remove_reference(Type const * type) {
  if (!type) return type;
  if (auto ref_type = std::get_if<ReferenceType>(&type->v)) {
    type = ref_type->references.get();
  }
  return type;
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
    pattern(as<ReferenceType>(arg)) = [&](auto& reference_type) {
      auto [referenced_type, symbol] = resolve_type(scope, reference_type.references);
      reference_type.references = referenced_type;
      return std::make_pair(type, symbol);
    },
    pattern(_) = [&] {
      return std::make_pair(Type_Ptr(nullptr), std::optional<Ast_Identifier>{});
    });
}
