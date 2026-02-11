cmake_minimum_required(VERSION 3.28)
project(pdh_02_query_selector)

set(CMAKE_CXX_STANDARD 20)

add_executable(pdh_02_query_selector main.cpp)
target_link_libraries(pdh_02_query_selector PRIVATE Pdh)