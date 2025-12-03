cmake_minimum_required(VERSION 3.29)
project(dwrite-base CXX)

add_executable(dwrite-base WIN32
    main.cpp
        App.cpp
        App.h
        DWrite.cpp
        DWrite.h
        Capturer.cpp
        Capturer.h)

target_link_libraries(dwrite-base PRIVATE dwrite.lib)
target_link_libraries(dwrite-base PRIVATE dxgi.lib)
target_link_libraries(dwrite-base PRIVATE d3d11.lib)
target_link_libraries(dwrite-base PRIVATE d2d1.lib)

find_package(directxtex CONFIG REQUIRED)
target_link_libraries(dwrite-base PRIVATE Microsoft::DirectXTex)