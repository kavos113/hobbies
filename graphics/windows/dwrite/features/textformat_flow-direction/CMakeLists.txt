cmake_minimum_required(VERSION 3.29)
project(dwrite-format-flow-direction CXX)

add_executable(dwrite-format-flow-direction WIN32
    main.cpp
        App.cpp
        App.h
        DWrite.cpp
        DWrite.h
        Capturer.cpp
        Capturer.h)

target_link_libraries(dwrite-format-flow-direction PRIVATE dwrite.lib)
target_link_libraries(dwrite-format-flow-direction PRIVATE dxgi.lib)
target_link_libraries(dwrite-format-flow-direction PRIVATE d3d11.lib)
target_link_libraries(dwrite-format-flow-direction PRIVATE d2d1.lib)

find_package(directxtex CONFIG REQUIRED)
target_link_libraries(dwrite-format-flow-direction PRIVATE Microsoft::DirectXTex)