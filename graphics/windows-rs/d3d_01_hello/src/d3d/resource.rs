use windows::Win32::Foundation::RECT;
use windows::Win32::Graphics::Direct3D12::{
    ID3D12CommandAllocator, ID3D12CommandQueue, ID3D12DescriptorHeap, ID3D12Fence,
    ID3D12GraphicsCommandList, ID3D12PipelineState, ID3D12Resource, ID3D12RootSignature,
    D3D12_VERTEX_BUFFER_VIEW, D3D12_VIEWPORT,
};
use windows::Win32::Graphics::Dxgi::IDXGISwapChain4;

pub struct Resources {
    commands: Commands,
    display: Display,
    vertex_resource: VertexResource,
    fence: Fence,
    pipeline: Pipeline,
    view_port: D3D12_VIEWPORT,
    scissor_rect: RECT,
}

struct Commands {
    command_allocator: ID3D12CommandAllocator,
    command_queue: ID3D12CommandQueue,
    command_list: ID3D12GraphicsCommandList,
}

struct Display {
    swap_chain: IDXGISwapChain4,
    back_buffers: Vec<ID3D12Resource>,
    rtv_heap: ID3D12DescriptorHeap,
}

impl Display {
    const BACK_BUFFER_COUNT: usize = 2;
    const CLEAR_COLOR: [f32; 4] = [0.0, 0.2, 0.4, 1.0];
}

struct Fence {
    fence: ID3D12Fence,
    value: u64,
}

struct Pipeline {
    root_signature: ID3D12RootSignature,
    pipeline: ID3D12PipelineState,
}

struct Vertex {
    position: [f32; 3],
    color: [f32; 4],
}

struct VertexResource {
    vertex_buffer: ID3D12Resource,
    vertex_buffer_view: D3D12_VERTEX_BUFFER_VIEW,
}
