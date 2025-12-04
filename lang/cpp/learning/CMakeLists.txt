message(STATUS "path env: " $ENV{PATH})
cmake_minimum_required(VERSION 3.18 FATAL_ERROR)
project(othello-learn)


find_package(Torch REQUIRED)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${TORCH_CXX_FLAGS}")

add_executable(othello-learn
        main.cpp
        othello_board.h
        network.h
        learn.cpp
)
target_link_libraries(othello-learn "${TORCH_LIBRARIES}")
set_property(TARGET othello-learn PROPERTY CXX_STANDARD 20)

if (MSVC)
    file(GLOB TORCH_DLLS "${TORCH_INSTALL_PREFIX}/lib/*.dll")
    add_custom_command(TARGET othello-learn
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${TORCH_DLLS}
            $<TARGET_FILE_DIR:othello-learn>)
endif ()