cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME 23_refactor)
project(d3d_${EXAMPLE_NAME})

set(CMAKE_CXX_STANDARD 20)

add_executable(d3d_${EXAMPLE_NAME}
        main.cpp
        Application.cpp
        D3DEngine.cpp
        Debug.cpp
        Model.cpp
)
target_link_libraries(d3d_${EXAMPLE_NAME} PRIVATE d3d12 dxgi d3dcompiler)
target_compile_definitions(d3d_${EXAMPLE_NAME} PRIVATE DEBUG)
target_include_directories(d3d_${EXAMPLE_NAME} PRIVATE ${THIRD_PARTY_DIR})

set_target_properties(d3d_${EXAMPLE_NAME} PROPERTIES FOLDER d3d)

find_package(directxmath CONFIG REQUIRED)
target_link_libraries(d3d_${EXAMPLE_NAME} PRIVATE Microsoft::DirectXMath)

find_package(directxtex CONFIG REQUIRED)
target_link_libraries(d3d_${EXAMPLE_NAME} PRIVATE Microsoft::DirectXTex)

file(COPY shader.hlsl DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY ${D3D_RESOURCES_DIR}/texture.png DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY ${D3D_RESOURCES_DIR}/models/ DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/)