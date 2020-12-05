find_package(Boost COMPONENTS program_options REQUIRED)

include_directories(
  ../parser/include
  ../semantics/include
  ../codegen/include
  ${Boost_INCLUDE_DIR}
)

add_executable(mankc
  main.cpp
  compiler_message_printer.cpp)

target_link_libraries(mankc
  MankAST
  MankParser
  MankSemantics
  # MankCodegen
  ${Boost_LIBRARIES})

set_target_properties(mankc
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
