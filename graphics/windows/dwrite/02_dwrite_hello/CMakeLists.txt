cmake_minimum_required(VERSION 3.28)
project(dwrite_02_dwrite_hello)

set(CMAKE_CXX_STANDARD 20)

add_executable(dwrite_02_dwrite_hello
        main.cpp
        Application.cpp
        DWriteEngine.cpp
)
target_link_libraries(dwrite_02_dwrite_hello PRIVATE d2d1 d3d11 dxgi dwrite)