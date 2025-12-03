cmake_minimum_required(VERSION 3.29)
project(dwrite-format-word-wrapping CXX)

add_executable(dwrite-format-word-wrapping WIN32
    main.cpp
        App.cpp
        App.h
        DWrite.cpp
        DWrite.h
        Capturer.cpp
        Capturer.h)

target_link_libraries(dwrite-format-word-wrapping PRIVATE dwrite.lib)
target_link_libraries(dwrite-format-word-wrapping PRIVATE dxgi.lib)
target_link_libraries(dwrite-format-word-wrapping PRIVATE d3d11.lib)
target_link_libraries(dwrite-format-word-wrapping PRIVATE d2d1.lib)

find_package(directxtex CONFIG REQUIRED)
target_link_libraries(dwrite-format-word-wrapping PRIVATE Microsoft::DirectXTex)