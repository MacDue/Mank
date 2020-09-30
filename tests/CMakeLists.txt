include_directories(
  ${PROJECT_SOURCE_DIR}/test
  ${PROJECT_SOURCE_DIR}/src)

add_executable(UnitTests tests_main.cpp)
target_link_libraries(UnitTests MankCore)
add_test(Name MankTests COMMAND UnitTests)
