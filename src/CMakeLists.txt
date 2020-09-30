add_library(MankCore
  lexer.cpp
  parser.cpp
)

include_directories(
  include
)

target_link_libraries(MankCore
  formatxx
  mpark_patterns
)

add_executable(mankc main.cpp)
target_link_libraries(mankc MankCore)
