cmake_minimum_required(VERSION 3.28)
project(d3d_35_descriptor_management)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(d3d_35_descriptor_management
        main.cpp
        Application.cpp
        D3DEngine.cpp
        Debug.cpp
        Model.cpp
        DescriptorHeap.cpp
)
target_link_libraries(d3d_35_descriptor_management PRIVATE d3d12 dxgi d3dcompiler)
target_compile_definitions(d3d_35_descriptor_management PRIVATE DEBUG)
target_include_directories(d3d_35_descriptor_management PRIVATE ${THIRD_PARTY_DIR})

find_package(directxmath CONFIG REQUIRED)
target_link_libraries(d3d_35_descriptor_management PRIVATE Microsoft::DirectXMath)

find_package(directxtex CONFIG REQUIRED)
target_link_libraries(d3d_35_descriptor_management PRIVATE Microsoft::DirectXTex)

find_package(D3D12MemoryAllocator CONFIG REQUIRED)
target_link_libraries(d3d_35_descriptor_management PRIVATE GPUOpen::D3D12MemoryAllocator)

file(COPY shader.hlsl DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY ${D3D_RESOURCES_DIR}/texture.png DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY ${D3D_RESOURCES_DIR}/models/ DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/)