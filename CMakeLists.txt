cmake_minimum_required (VERSION 3.6)

project(Mank)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS_DEBUG "-g -Wall -Wextra -Wno-init-list-lifetime")

add_subdirectory(src)
# add_subdirectory(deps/litexx)
# add_subdirectory(deps/formatxx)

enable_testing ()
add_subdirectory(tests)
