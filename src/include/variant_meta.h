#define VARIANT_META_FOR(type)                                                     \
namespace std {                                                                    \
  template <>                                                                      \
  struct variant_size<type> : variant_size<type::variant> {};                      \
  template <std::size_t I>                                                         \
  struct variant_alternative<I, type> : variant_alternative<I, type::variant> {};  \
}
