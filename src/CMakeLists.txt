add_library(MankCore
  lexer.cpp
  parser.cpp
  ast_printer.cpp
  compiler_error_printer.cpp
  token_helpers.cpp
)

find_package(Boost COMPONENTS program_options REQUIRED)

include_directories(
  include
  ${Boost_INCLUDE_DIR}
)

target_link_libraries(MankCore
  formatxx
  mpark_patterns
)

message(${Boost_LIBRARIES})

add_executable(mankc main.cpp)
target_link_libraries(mankc
  MankCore
  ${Boost_LIBRARIES})
