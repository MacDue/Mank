# TODO: Fix these includes
include_directories(
  ${PROJECT_SOURCE_DIR}/test
  ${PROJECT_SOURCE_DIR}/src/analysis/include
  ${PROJECT_SOURCE_DIR}/src/codegen/include
)

add_executable(UnitTests
  tests_main.cpp
  simple_lexing.cpp
  parsing.cpp
  simple_semantics.cpp
  reachability.cpp
  codegen.cpp)

target_link_libraries(UnitTests MankAnalysis MankCodegen)
add_test(Name MankTests COMMAND UnitTests)
