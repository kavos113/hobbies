cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME 31_compute_shader)
project(d3d_${EXAMPLE_NAME})

set(CMAKE_CXX_STANDARD 20)

add_executable(d3d_${EXAMPLE_NAME}
        main.cpp
        D3DEngine.cpp
)
target_link_libraries(d3d_${EXAMPLE_NAME} PRIVATE d3d12 dxgi d3dcompiler)
target_compile_definitions(d3d_${EXAMPLE_NAME} PRIVATE DEBUG)

file(COPY cs.hlsl DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set_target_properties(d3d_${EXAMPLE_NAME} PROPERTIES FOLDER d3d)