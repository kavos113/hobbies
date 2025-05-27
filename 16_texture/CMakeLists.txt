cmake_minimum_required(VERSION 3.28)
project(texture)

set(CMAKE_CXX_STANDARD 20)

add_executable(texture
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
target_include_directories(texture PRIVATE $ENV{LIBRARY_DIR}/glfw-3.4.bin.WIN64/include)
target_include_directories(texture PRIVATE $ENV{LIBRARY_DIR}/glm-1.0.1-light)
target_include_directories(texture PRIVATE $ENV{VULKAN_SDK}/Include)
target_link_libraries(texture PRIVATE $ENV{LIBRARY_DIR}/glfw-3.4.bin.WIN64/lib-vc2022/glfw3.lib)
target_link_libraries(texture PRIVATE $ENV{VULKAN_SDK}/Lib/vulkan-1.lib)
target_include_directories(texture PRIVATE ${THIRD_PARTY_DIR})

file(COPY ${RESOURCE_DIR}/square.png DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_custom_command(
        TARGET texture
        PRE_BUILD
        COMMAND $ENV{VULKAN_SDK}/Bin/glslc.exe ${CMAKE_CURRENT_SOURCE_DIR}/shaders/shader.vert -o ${CMAKE_CURRENT_SOURCE_DIR}/shaders/vert.spv
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/shaders/vert.spv ${CMAKE_CURRENT_BINARY_DIR}/vert.spv
        COMMENT "Compiling vertex shader"
)
add_custom_command(
        TARGET texture
        PRE_BUILD
        COMMAND $ENV{VULKAN_SDK}/Bin/glslc.exe ${CMAKE_CURRENT_SOURCE_DIR}/shaders/shader.frag -o ${CMAKE_CURRENT_SOURCE_DIR}/shaders/frag.spv
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/shaders/frag.spv ${CMAKE_CURRENT_BINARY_DIR}/frag.spv
        COMMENT "Compiling fragment shader"
)
