# Vulkan Examples

required vcpkg packages:

- glfw3
- glm
- vulkan memory allocator
- tinyobjloader
- shader-slang

other: 
- vulkan sdk

TODO
- 15_1: independent transfer queue
- renderpass fallback

## All examples

| number | name                   | base | description                                           | lines |
|--------|------------------------|------|-------------------------------------------------------|-------|
| 1      | hello_glfw             | -    | create a window with glfw                             | 80    |
| 2      | create_instance        | 1    | create a vulkan instance                              | 221   |
| 3      | debug_callback         | 2    | setup a debug callback with VK_EXT_debug_utils        | 396   |
| 4      | physical_device        | 3    | select a physical device                              | 556   |
| 5      | logical_device         | 4    | create a logical device                               | 559   |
| 6      | surface                | 5    | create a surface for presentation                     | 585   |
| 7      | swapchain              | 6    | create a swapchain                                    | 688   |
| 8      | image_view             | 7    | create image views for the swapchain images           | 733   |
| 9      | shaders                | 8    | slang shaders to draw a triangle                      | 860   |
| 10     | pipeline               | 9    | create a graphics pipeline                            | 981   |
| 11     | command_buffer         | 10   | create, record command buffers                        | 1143  |
| 12     | present                | 11   | present the rendered image to the screen              | 1266  |
| 13     | recreate_swapchain     | 12   | handle window resizing by recreating the swapchain    | 1317  |
| 14     | vertex_buffer          | 13   | create a vertex buffer of a triangle                  | 1425  |
| 15     | staging_buffer         | 14   | use a staging buffer to transfer vertex data          | 1493  |
| 16     | index_buffer           | 15   | create an index buffer, and draw a rectangle          | 1534  |
| 17     | uniform_buffer         | 16   | create a uniform buffer (data: transform matrix)      | 1721  |
| 18     | refactor               | 17   | split device/instance and more as VulkanContext class | 1875  |
| 19     | texture                | 18   | load texture                                          | 2192  |
| 20     | dynamic_uniform_buffer | 19   | use UNIFORM_BUFFER_DYNAMIC in matrix uniform buffer   | 2257  |
| 21     | buffer_struct          | 19   | create VulkanBuffer struct (part of refactor)         | 2388  |
| 22     | depth                  | 21   | add depth stencil buffer                              | 2489  |