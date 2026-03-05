cmake_minimum_required(VERSION 3.28)
project(yoga-hello)

set(CMAKE_CXX_STANDARD 20)

add_executable(yoga-hello main.cpp)

find_package(yoga CONFIG REQUIRED)
target_link_libraries(yoga-hello PRIVATE yoga::yogacore)
