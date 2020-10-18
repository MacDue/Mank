#pragma once

// See: https://stackoverflow.com/a/16000226/3818491

template <typename T, typename = int>
struct is_abstract_ast : std::false_type {};

template <typename T>
struct is_abstract_ast<T, decltype((void) T::v, 0)> : std::true_type {};
