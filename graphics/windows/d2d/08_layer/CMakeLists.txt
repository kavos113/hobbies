cmake_minimum_required(VERSION 3.28)
project(d2d_08_layer)

set(CMAKE_CXX_STANDARD 20)

add_executable(d2d_08_layer
        main.cpp
        Application.cpp
)
target_link_libraries(d2d_08_layer PRIVATE d2d1 d3d11 dxgi)