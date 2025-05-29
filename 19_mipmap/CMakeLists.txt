cmake_minimum_required(VERSION 3.28)
project(mipmap)

set(CMAKE_CXX_STANDARD 20)

add_executable(mipmap
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
target_include_directories(mipmap PRIVATE $ENV{LIBRARY_DIR}/glfw-3.4.bin.WIN64/include)
target_include_directories(mipmap PRIVATE $ENV{LIBRARY_DIR}/glm-1.0.1-light)
target_include_directories(mipmap PRIVATE $ENV{VULKAN_SDK}/Include)
target_link_libraries(mipmap PRIVATE $ENV{LIBRARY_DIR}/glfw-3.4.bin.WIN64/lib-vc2022/glfw3.lib)
target_link_libraries(mipmap PRIVATE $ENV{VULKAN_SDK}/Lib/vulkan-1.lib)
target_include_directories(mipmap PRIVATE ${THIRD_PARTY_DIR})

file(COPY ${RESOURCE_DIR}/models/utility_box.obj DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY ${RESOURCE_DIR}/models/utility_box.mtl DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY ${RESOURCE_DIR}/models/textures/utility_box_02_diff_1k.jpg DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/textures)
file(COPY ${RESOURCE_DIR}/models/textures/utility_box_02_arm_1k.jpg DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/textures)
file(COPY ${RESOURCE_DIR}/models/textures/utility_box_02_nor_gl_1k.jpg DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/textures)

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
        shaders_mipmap ALL
        DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/shaders/vert.spv
        ${CMAKE_CURRENT_SOURCE_DIR}/shaders/frag.spv
)

add_dependencies(mipmap shaders_mipmap)
