cmake_minimum_required(VERSION 4.2)
project(cpp_visu)

set(CMAKE_CXX_STANDARD 20)

add_executable(cpp_visu main.cpp)
target_include_directories(cpp_visu PRIVATE $ENV{LLVM_DIR}/include)
target_link_directories(cpp_visu PRIVATE $ENV{LLVM_DIR}/lib)
target_link_libraries(cpp_visu libclang)