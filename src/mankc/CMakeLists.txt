find_package(Boost COMPONENTS program_options REQUIRED)

include_directories(
  ../analysis/include
  ../codegen/include
  ${Boost_INCLUDE_DIR}
)

add_executable(mankc main.cpp)
target_link_libraries(mankc
  MankAnalysis
  MankCodegen
  ${Boost_LIBRARIES})

set_target_properties(mankc
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
