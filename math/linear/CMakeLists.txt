cmake_minimum_required(VERSION 3.28)
project(linear LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)

add_library(linear
        src/mat.h
        src/quartanion.h
)
set_target_properties(linear PROPERTIES LINKER_LANGUAGE CXX)

add_executable(example_matrix examples/matrix.cpp)
add_executable(example_quartanion examples/quartanion.cpp)