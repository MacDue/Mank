add_library(MankSemantics
  semantics.cpp
  return_reachability.cpp
  const_propagate.cpp
  types.cpp
  binding_helpers.cpp
  macros.cpp
  bind_macro.cpp
  curry_macro.cpp
)

target_link_libraries(MankSemantics
  mpark_patterns
  MankParser
  formatxx
  MankAST
)
