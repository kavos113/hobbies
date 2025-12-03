cmake_minimum_required(VERSION 3.28)
project(cairo_hello)

find_package(PkgConfig REQUIRED)
pkg_check_modules(CAIRO REQUIRED cairo)

message("CAIRO_INCLUDE_DIRS: ${CAIRO_INCLUDE_DIRS}")
message("CAIRO_LIBRARIES: ${CAIRO_LIBRARIES}")
message("CAIRO_LIBRARY_DIRS: ${CAIRO_LIBRARY_DIRS}")
message("CAIRO_CFLAGS_OTHER: ${CAIRO_CFLAGS_OTHER}")

add_executable(cairo_hello main.cpp)
target_include_directories(cairo_hello PRIVATE ${CAIRO_INCLUDE_DIRS})
target_link_libraries(cairo_hello PRIVATE ${CAIRO_LIBRARIES})
target_link_directories(cairo_hello PRIVATE ${CAIRO_LIBRARY_DIRS})
target_compile_options(cairo_hello PRIVATE ${CAIRO_CFLAGS_OTHER})
