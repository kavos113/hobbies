add_executable(freetype_ft-dwrite-metrics
        main.cpp
)

find_package(Freetype REQUIRED)
target_link_libraries(freetype_ft-dwrite-metrics PRIVATE Freetype::Freetype dwrite)