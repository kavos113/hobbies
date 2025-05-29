cmake_minimum_required(VERSION 3.28)
project(load_model)

set(CMAKE_CXX_STANDARD 20)

add_executable(load_model
        main.cpp
        Application.cpp
        Application.h
        debugUtils.h
        utils.h
        DebugManager.cpp
        DebugManager.h
        Swapchain.cpp
        Swapchain.h
        QueueFamilyIndices.h
        settings.h
        Command.cpp
        Command.h
)
target_include_directories(load_model PRIVATE $ENV{LIBRARY_DIR}/glfw-3.4.bin.WIN64/include)
target_include_directories(load_model PRIVATE $ENV{LIBRARY_DIR}/glm-1.0.1-light)
target_include_directories(load_model PRIVATE $ENV{VULKAN_SDK}/Include)
target_link_libraries(load_model PRIVATE $ENV{LIBRARY_DIR}/glfw-3.4.bin.WIN64/lib-vc2022/glfw3.lib)
target_link_libraries(load_model PRIVATE $ENV{VULKAN_SDK}/Lib/vulkan-1.lib)
target_include_directories(load_model PRIVATE ${THIRD_PARTY_DIR})

file(COPY ${RESOURCE_DIR}/square.png DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_custom_command(
        OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/shaders/vert.spv
        COMMAND $ENV{VULKAN_SDK}/Bin/glslc.exe ${CMAKE_CURRENT_SOURCE_DIR}/shaders/shader.vert -o ${CMAKE_CURRENT_SOURCE_DIR}/shaders/vert.spv
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/shaders/vert.spv ${CMAKE_CURRENT_BINARY_DIR}/vert.spv
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/shaders/shader.vert
        COMMENT "Compiling vertex shader"
)
add_custom_command(
        OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/shaders/frag.spv
        COMMAND $ENV{VULKAN_SDK}/Bin/glslc.exe ${CMAKE_CURRENT_SOURCE_DIR}/shaders/shader.frag -o ${CMAKE_CURRENT_SOURCE_DIR}/shaders/frag.spv
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/shaders/frag.spv ${CMAKE_CURRENT_BINARY_DIR}/frag.spv
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/shaders/shader.frag
        COMMENT "Compiling fragment shader"
)

add_custom_target(
        shaders_load_model ALL
        DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/shaders/vert.spv
        ${CMAKE_CURRENT_SOURCE_DIR}/shaders/frag.spv
)

add_dependencies(load_model shaders_load_model)
