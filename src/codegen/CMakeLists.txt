
add_library(MankCodegen
  codegen.cpp
  mangle.cpp
)

find_package(LLVM REQUIRED CONFIG)

include_directories(
  ${LLVM_INCLUDE_DIRS}
)

add_definitions(${LLVM_DEFINITIONS})

# TODO: Look into statically linking LLVM to allow the
# built compiler to run on more than just my machine (without a lot of installing)
target_link_libraries(MankCodegen
  mpark_patterns
  MankParser
  formatxx
  LLVM-10
)
