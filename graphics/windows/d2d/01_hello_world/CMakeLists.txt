cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME 01_hello_world)
project(d2d_${EXAMPLE_NAME})

set(CMAKE_CXX_STANDARD 20)

add_executable(d2d_${EXAMPLE_NAME}
        main.cpp
        Application.cpp
)
target_link_libraries(d2d_${EXAMPLE_NAME} PRIVATE d2d1)

set_target_properties(d2d_${EXAMPLE_NAME} PROPERTIES FOLDER d2d)
