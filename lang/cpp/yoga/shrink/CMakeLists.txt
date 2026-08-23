cmake_minimum_required(VERSION 3.28)

set(EXAMPLE_NAME shrink)
project(yoga-${EXAMPLE_NAME})

set(CMAKE_CXX_STANDARD 20)

add_executable(yoga-${EXAMPLE_NAME}
        main.cpp
)

find_package(yoga CONFIG REQUIRED)
target_link_libraries(yoga-${EXAMPLE_NAME} PRIVATE yoga::yogacore)

set_target_properties(yoga-${EXAMPLE_NAME} PROPERTIES FOLDER "yoga")
