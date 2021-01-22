cmake_minimum_required (VERSION 3.6)

# set(CMAKE_C_COMPILER "/usr/bin/gcc-10")
# set(CMAKE_CXX_COMPILER "/usr/bin/g++-10")

project(Mank)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpessimizing-move -Wno-init-list-lifetime")
set(CMAKE_CXX_FLAGS_DEBUG "-g -export-dynamic")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

if (CMAKE_BUILD_TYPE STREQUAL "Release")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -flto")
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

enable_testing ()
add_subdirectory(tests)
