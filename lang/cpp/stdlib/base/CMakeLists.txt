cmake_minimum_required(VERSION 3.28)

set(SAMPLE_NAME stdlib_base)

project(${SAMPLE_NAME})

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(${SAMPLE_NAME}
    main.cpp
)
