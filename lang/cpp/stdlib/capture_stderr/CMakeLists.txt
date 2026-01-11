cmake_minimum_required(VERSION 3.28)
project(stdlib_capture_stderr)

set(CMAKE_CXX_STANDARD 20)

add_executable(stdlib_capture_stderr_cpp only_cpp.cpp)
add_executable(stdlib_capture_stderr_c_windows c_windows.cpp)
