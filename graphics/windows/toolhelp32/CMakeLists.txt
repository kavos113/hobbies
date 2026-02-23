cmake_minimum_required(VERSION 3.28)
project(toolhelp32)

set(CMAKE_CXX_STANDARD 20)

add_executable(toolhelp32_process_info process_info.cpp)
add_executable(toolhelp32_module_info module_info.cpp)
add_executable(toolhelp32_thread_info thread_info.cpp)
add_executable(toolhelp32_heap_info heap_info.cpp)
