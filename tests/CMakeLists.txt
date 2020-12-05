add_executable(UnitTests
  tests_main.cpp
  simple_lexing.cpp
  parsing.cpp
  simple_semantics.cpp
  reachability.cpp
  inference.cpp
  codegen.cpp)

target_link_libraries(UnitTests
  MankAST
  MankParser
  MankSemantics
  MankCodegen
  gc)

add_test(Name MankTests COMMAND UnitTests)
