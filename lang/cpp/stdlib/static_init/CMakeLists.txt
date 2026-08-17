cmake_minimum_required(VERSION 3.28)

set(SAMPLE_NAME static_init)
project(stdlib_${SAMPLE_NAME})

set(CMAKE_CXX_STANDARD 20)

add_executable(stdlib_${SAMPLE_NAME} main.cpp)
set_target_properties(stdlib_${SAMPLE_NAME} PROPERTIES FOLDER stdlib)
