
add_library(MankCodegen
  codegen.cpp
)

find_package(LLVM REQUIRED CONFIG)

include_directories(
  ${LLVM_INCLUDE_DIRS}
  include
)

add_definitions(${LLVM_DEFINITIONS})

target_link_libraries(MankCodegen LLVM-10)
