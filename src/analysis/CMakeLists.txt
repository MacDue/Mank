add_library(MankAnalysis
  lexer.cpp
  parser.cpp
  ast_printer.cpp
  compiler_error_printer.cpp
  token_helpers.cpp
  semantics.cpp
  types.cpp
  return_reachability.cpp
  default_ast_visitor.cpp
)

include_directories(
  include
)

target_link_libraries(MankAnalysis
  formatxx
  mpark_patterns
)

