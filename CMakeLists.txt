cmake_minimum_required (VERSION 3.6)

# set(CMAKE_C_COMPILER "/usr/bin/gcc-10")
# set(CMAKE_CXX_COMPILER "/usr/bin/g++-10")

project(Mank)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpessimizing-move -Wno-init-list-lifetime -fexceptions")
set(CMAKE_CXX_FLAGS_DEBUG "-g -export-dynamic")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fexceptions")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

option(ENABLE_UBSAN "Enable undefined behaviour sanitizer" OFF)
option(WASM_BUILD "Enable WASM/Emscripten specific configurations" OFF)

if (ENABLE_UBSAN)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=undefined")
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Release")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -flto")
endif()

if (NOT WASM_BUILD)
  # The wasm build can't do JIT
  add_compile_definitions(MANK_ENABLE_JIT MANK_CODEGEN_PRINT_IR)
endif()

include_directories(
  ./src/ast/include
  ./src/parser/include
  ./src/semantics/include
  ./src/codegen/include
  ./src/util/include
)

add_subdirectory(src)
add_subdirectory(deps/litexx)
add_subdirectory(deps/formatxx)
add_subdirectory(deps/patterns)
add_subdirectory(deps/ordered-map)

set(WITH_TESTS OFF CACHE BOOL "Enable tests" FORCE)
add_subdirectory(deps/eternal)

enable_testing ()
add_subdirectory(tests)
