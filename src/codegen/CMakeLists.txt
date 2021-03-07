
add_library(MankCodegen
  codegen.cpp
  mangle.cpp
)

set(LLVM_DIR "/usr/local/lib/llvm-10-wasm/lib/cmake/llvm")

find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs support core irreader)

# TODO: Look into statically linking LLVM to allow the
# built compiler to run on more than just my machine (without a lot of installing)
target_link_libraries(MankCodegen
  mpark_patterns
  MankParser
  formatxx
  ${llvm_libs}
  # LLVM-10
)
