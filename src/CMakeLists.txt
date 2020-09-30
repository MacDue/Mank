add_library(MankCore
  include/lexer.h
  lexer.cpp)
# target_link_libraries(MankCore formatxx)
add_executable(mankc main.cpp)
target_link_libraries(mankc MankCore)
