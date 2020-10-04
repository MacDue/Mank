cmake_minimum_required (VERSION 3.6)

project(Mank)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpessimizing-move -Wno-init-list-lifetime")
set(CMAKE_CXX_FLAGS_DEBUG "-g")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

add_subdirectory(src)
add_subdirectory(deps/litexx)
add_subdirectory(deps/formatxx)
add_subdirectory(deps/patterns)

enable_testing ()
add_subdirectory(tests)
