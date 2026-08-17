cmake_minimum_required(VERSION 3.28)

set(SAMPLE_NAME capture_stderr)
project(stdlib_${SAMPLE_NAME})

set(CMAKE_CXX_STANDARD 20)

add_executable(stdlib_${SAMPLE_NAME}_cpp only_cpp.cpp)
add_executable(stdlib_${SAMPLE_NAME}_c_windows c_windows.cpp)
set_target_properties(stdlib_${SAMPLE_NAME}_cpp PROPERTIES FOLDER stdlib)
set_target_properties(stdlib_${SAMPLE_NAME}_c_windows PROPERTIES FOLDER stdlib)
