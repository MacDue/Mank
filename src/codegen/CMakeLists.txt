
add_library(MankCodegen
  codegen.cpp
  mangle.cpp
)

if (WASM_BUILD)
  # A specially built wasm version of LLVM 10
  set(LLVM_DIR "/usr/local/lib/llvm-10-wasm/lib/cmake/llvm")
endif()

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
)

if (WASM_BUILD)
  target_link_libraries(MankCodegen ${llvm_libs})
else()
  target_link_libraries(MankCodegen LLVM-10)
endif()
