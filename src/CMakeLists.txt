
link_libraries(
  formatxx
  mpark_patterns
  tsl::ordered_map
)

add_subdirectory(ast)
add_subdirectory(parser)
add_subdirectory(semantics)
add_subdirectory(codegen)
add_subdirectory(mankc)
