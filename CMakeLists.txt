cmake_minimum_required(VERSION 3.31)
project(rt1w)

set(CMAKE_CXX_STANDARD 20)

add_executable(rt1w main.cpp
        src/sphere.cpp
        src/hittable_list.cpp
)
