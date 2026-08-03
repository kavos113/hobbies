cmake_minimum_required(VERSION 3.28)
project(stdlib_static_init)

set(CMAKE_CXX_STANDARD 20)

add_executable(stdlib_static_init main.cpp)
set_target_properties(stdlib_ PROPERTIES FOLDER stdlib)
