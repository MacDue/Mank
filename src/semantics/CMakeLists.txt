add_library(MankSemantics
  semantics.cpp
  return_reachability.cpp
  const_propagate.cpp
  types.cpp
  binding_helpers.cpp
  macros.cpp
  bind_macro.cpp
  curry_macro.cpp
  type_infer.cpp
  print_macro.cpp
  assert_macro.cpp
  vector_literal_macro.cpp
  builtin_functions.cpp
)

target_link_libraries(MankSemantics
  mpark_patterns
  MankParser
  formatxx
  MankAST
)

if (WASM_BUILD)
  set_target_properties(MankSemantics PROPERTIES COMPILE_FLAGS "-s USE_BOOST_HEADERS=1")
endif()
