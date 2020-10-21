add_library(MankSemantics
  semantics.cpp
  return_reachability.cpp
  const_propagate.cpp
)

target_link_libraries(MankSemantics
  mpark_patterns
  MankParser
  formatxx
  MankAST
)
