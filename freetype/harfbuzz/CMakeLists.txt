add_executable(freetype_hello_harfbuzz
        main.cpp
)

find_package(Freetype REQUIRED)
target_link_libraries(freetype_hello_harfbuzz PRIVATE Freetype::Freetype)

find_package(harfbuzz REQUIRED)
target_link_libraries(freetype_hello_harfbuzz PRIVATE harfbuzz::harfbuzz)