# Windows API Examples

vcpkg with manifest mode

## Direct3D12

| no, | name                  | base | description                                                                    | lines |
|-----|-----------------------|------|--------------------------------------------------------------------------------|-------|
| 1   | hello_world           | -    | show window with HWND                                                          | 212   |
| 2   | debug_layer           | 1    | enable debug layer                                                             | 241   |
| 3   | adapter               | 2    | list and choose adapter                                                        | 350   |
| 4   | create_device         | 3    | create ID3D12Device                                                            | 369   |
| 5   | command_queue         | 4    | create command related resources                                               | 416   |
| 6   | swap_chain            | 5    | create swapchain and get back buffers                                          | 504   |
| 7   | present               | 6    | create fence, and add per-frame process                                        | 675   |
| 8   | custom_debug          | 7    | add custom debug callback                                                      | 828   | 
| 9   | vertex_buffer         | 8    | create vertex buffer                                                           | 940   |
| 10  | shaders               | 9    | add shaders, and compiling process                                             | 1015  |
| 11  | pipeline              | 10   | create graphics pipeline. can show very first triangle                         | 1170  |
| 12  | index_buffer          | 11   | add index buffer to show rectangle                                             | 1237  |
| 13  | descriptor            | 12   | create descriptor to use constant buffer, color of rectangle                   | 1337  |
| 14  | load_texture          | 13   | load texture image using DirectXTex                                            | 1440  |
| 15  | texture_view          | 14   | use texture image in shader                                                    | 1505  |
| 16  | default_heap          | 15   | use default heap(GPU memory) and copy from CPU memory for texture image buffer | 1661  | 
| 17  | more_default          | 16   | use default heap for vertex, index buffer                                      | 1729  |
| 18  | frame_buffering       | 17   | command allocator and fence has FRAME_COUNT resources to not wait              | 1753  |
| 19  | depth_resource        | 18   | use depth buffer                                                               | 1850  |
| 20  | load_model            | 19   | load .obj model with tinyobjloader                                             | 1909  |
| 21  | transform_matrix      | 20   | move model with matrix constant buffer                                         | 1941  |
| 22  | lighting              | 21   | implement lambertian reflectance                                               | 2035  |
| 23  | refactor              | 22   | split object resources                                                         | 2184  | 
| 24  | multi_path            | 23   | draw outline of model using multi-path rendering                               | 2321  |
| 25  | offscreen             | 23   | grayscale postprocess using offscreen rendering                                | 2650  |
| 26  | offscreen_vertex_id   | 25   | use TRIANGLESTRIP and use vertex_id                                            | 2500  |
| 27  | multi_render_target   | 23   | simple differed rendering using multi render target                            | 2596  |
| 28  | msaa                  | 23   | multi sampling anti aliasing                                                   | 2291  |
| 29  | instancing            | 23   | use instancing to render many objects                                          | 2287  |
| 30  | d3d12ma               | 23   | use D3D12MemoryAllocator                                                       | 2216  |
| 31  | compute_shader        | 4    | directx compute shader                                                         | 641   |
| 32  | dxr_init              | 9    | draw triangle using DirectX Raytracing                                         | 1736  |
| 33  | dxr_instancing        | 32   | instancing and draw multiple triangle                                          | 2317  |
| 34  | dxr_model             | 33   | draw model and render shadow                                                   | 2882  |
| 35  | descriptor_management | 30   | implement descriptor allocator and manager using copy strategy                 | 2589  |


## Direct2D