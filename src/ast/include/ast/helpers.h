#pragma once

#include <variant>

template<typename TCast, typename TVariant>
TCast& variant_cast(TVariant& variant) {
  return std::visit([](TCast& const_expr) -> TCast& {
    return const_expr;
  }, variant);
}
