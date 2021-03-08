include_directories(
  ../parser/include
  ../semantics/include
  ../codegen/include
)

if (NOT WASM_BUILD)
  find_package(Boost COMPONENTS program_options REQUIRED)
  include_directories(${Boost_INCLUDE_DIR})
endif()

add_executable(mankc
  main.cpp
  compiler_message_printer.cpp)

target_link_libraries(mankc
  MankParser
  MankSemantics
  MankCodegen
)

if (NOT WASM_BUILD)
  target_link_libraries(mankc ${Boost_LIBRARIES})
endif()

add_executable(mankc_web
  main.cpp
  compiler_message_printer.cpp)

target_link_libraries(mankc_web
  MankParser
  MankSemantics
  MankCodegen
)

set_target_properties(mankc_web
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}"
  LINK_FLAGS "--bind -s MODULARIZE=1 -s 'EXPORT_NAME=\"Mankc\"' --closure 0")

set_target_properties(mankc
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
