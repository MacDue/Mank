
add_library(MankCodegen
  codegen.cpp
)

find_package(LLVM REQUIRED CONFIG)

include_directories(
  ${LLVM_INCLUDE_DIRS}
  ../analysis/include
  include
)

add_definitions(${LLVM_DEFINITIONS})

# TODO: Look into statically linking LLVM to allow the
# built compiler to run on more than just my machine (without a lot of installing)
target_link_libraries(MankCodegen
  LLVM-10
  formatxx
  mpark_patterns
  MankAnalysis
)
