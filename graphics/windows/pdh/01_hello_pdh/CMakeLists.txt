cmake_minimum_required(VERSION 3.28)
project(pdh_01_hello_pdh)

set(CMAKE_CXX_STANDARD 20)

add_executable(pdh_01_hello_pdh main.cpp)
target_link_libraries(pdh_01_hello_pdh PRIVATE Pdh)