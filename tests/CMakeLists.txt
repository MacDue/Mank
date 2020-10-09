include_directories(
  ${PROJECT_SOURCE_DIR}/test
  ${PROJECT_SOURCE_DIR}/src/analysis/include
)

add_executable(UnitTests
  tests_main.cpp
  simple_lexing.cpp
  parsing.cpp
  simple_semantics.cpp)
target_link_libraries(UnitTests MankAnalysis)
add_test(Name MankTests COMMAND UnitTests)
