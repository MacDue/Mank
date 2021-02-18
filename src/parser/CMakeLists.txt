add_library(MankParser
  lexer.cpp
  parser.cpp
  token_helpers.cpp
)

# Using target_link_libraries is COMPLETELY broken -- this works
target_include_directories(MankParser PRIVATE "../../deps/eternal/include")
