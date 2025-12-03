add_executable(freetype_hello_world
        main.cpp
)

find_package(Freetype REQUIRED)
target_link_libraries(freetype_hello_world PRIVATE Freetype::Freetype)