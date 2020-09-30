add_library(MankCore
  include/lexer.h
  lexer.cpp)

include_directories(
  include)
  # target_link_libraries(MankCore formatxx)
add_executable(mankc main.cpp)
target_link_libraries(mankc MankCore)
