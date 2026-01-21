cmake_minimum_required(VERSION 3.28)
project(dwrite_03_draw_glyphrun)

set(CMAKE_CXX_STANDARD 20)

add_executable(dwrite_03_draw_glyphrun
        main.cpp
        Application.cpp
        DWriteEngine.cpp
)
target_link_libraries(dwrite_03_draw_glyphrun PRIVATE d2d1 d3d11 dxgi dwrite)