required vcpkg packages:

- glfw3
- glm
- vulkan memory allocator

other: 
- vulkan sdk

TODO
- 15_1: independent transfer queue
- renderpass fallback

## All examples

| number | name               | base | description                                        |
|--------|--------------------|------|----------------------------------------------------|
| 1      | hello_glfw         | -    | create a window with glfw                          |
| 2      | create_instance    | 1    | create a vulkan instance                           |
| 3      | debug_callback     | 2    | setup a debug callback with VK_EXT_debug_utils     |
| 4      | physical_device    | 3    | select a physical device                           |
| 5      | logical_device     | 4    | create a logical device                            |
| 6      | surface            | 5    | create a surface for presentation                  |
| 7      | swapchain          | 6    | create a swapchain                                 |
| 8      | image_view         | 7    | create image views for the swapchain images        |
| 9      | shaders            | 8    | slang shaders to draw a triangle                   |
| 10     | pipeline           | 9    | create a graphics pipeline                         |
| 11     | command_buffer     | 10   | create, record command buffers                     |
| 12     | present            | 11   | present the rendered image to the screen           |
| 13     | recreate_swapchain | 12   | handle window resizing by recreating the swapchain |
| 14     | vertex_buffer      | 13   | create a vertex buffer of a triangle               |
| 15     | staging_buffer     | 14   | use a staging buffer to transfer vertex data       |
| 16     | index_buffer       | 15   | create an index buffer, and draw a rectangle       |
| 17     | uniform_buffer     | 16   | create a uniform buffer (data: transform matrix)   |