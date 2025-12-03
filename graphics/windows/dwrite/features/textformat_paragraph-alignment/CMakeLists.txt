cmake_minimum_required(VERSION 3.29)
project(dwrite-format-paragraph-alignment CXX)

add_executable(dwrite-format-paragraph-alignment WIN32
    main.cpp
        App.cpp
        App.h
        DWrite.cpp
        DWrite.h
        Capturer.cpp
        Capturer.h)

target_link_libraries(dwrite-format-paragraph-alignment PRIVATE dwrite.lib)
target_link_libraries(dwrite-format-paragraph-alignment PRIVATE dxgi.lib)
target_link_libraries(dwrite-format-paragraph-alignment PRIVATE d3d11.lib)
target_link_libraries(dwrite-format-paragraph-alignment PRIVATE d2d1.lib)

find_package(directxtex CONFIG REQUIRED)
target_link_libraries(dwrite-format-paragraph-alignment PRIVATE Microsoft::DirectXTex)