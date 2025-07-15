cmake_minimum_required(VERSION 3.31)
project(rt1w)

set(CMAKE_CXX_STANDARD 20)

add_executable(rt1w main.cpp
        src/sphere.cpp
        src/hittable_list.cpp
)

find_package(OpenMP REQUIRED)
if (OPENMP_FOUND)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif ()