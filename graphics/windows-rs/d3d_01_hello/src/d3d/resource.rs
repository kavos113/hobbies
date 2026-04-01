use windows::core::Interface;
use windows::Win32::Foundation::{HWND, RECT};
use windows::Win32::Graphics::Direct3D12::{
    ID3D12CommandAllocator, ID3D12CommandQueue, ID3D12DescriptorHeap, ID3D12Device, ID3D12Fence,
    ID3D12GraphicsCommandList, ID3D12PipelineState, ID3D12Resource, ID3D12RootSignature,
    D3D12_COMMAND_LIST_TYPE_DIRECT, D3D12_COMMAND_QUEUE_DESC, D3D12_COMMAND_QUEUE_PRIORITY_NORMAL,
    D3D12_DESCRIPTOR_HEAP_DESC, D3D12_DESCRIPTOR_HEAP_TYPE_RTV, D3D12_VERTEX_BUFFER_VIEW,
    D3D12_VIEWPORT,
};
use windows::Win32::Graphics::Dxgi::Common::{
    DXGI_ALPHA_MODE_UNSPECIFIED, DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_SAMPLE_DESC,
};
use windows::Win32::Graphics::Dxgi::{
    IDXGIFactory7, IDXGISwapChain4, DXGI_SCALING_STRETCH, DXGI_SWAP_CHAIN_DESC1,
    DXGI_SWAP_EFFECT_FLIP_DISCARD, DXGI_USAGE_RENDER_TARGET_OUTPUT,
};
use windows::Win32::UI::WindowsAndMessaging::GetClientRect;

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
    const BACK_BUFFER_COUNT: u32 = 2;
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

impl Commands {
    fn new(device: &ID3D12Device) -> Self {
        let command_allocator: ID3D12CommandAllocator =
            match unsafe { device.CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT) } {
                Ok(allocator) => allocator,
                Err(hr) => panic!("Failed to create command allocator: {:?}", hr),
            };

        let command_list: ID3D12GraphicsCommandList = match unsafe {
            device.CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &command_allocator, None)
        } {
            Ok(list) => list,
            Err(hr) => panic!("Failed to create command list: {:?}", hr),
        };

        let queue_desc = D3D12_COMMAND_QUEUE_DESC {
            Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
            Priority: D3D12_COMMAND_QUEUE_PRIORITY_NORMAL.0,
            Flags: Default::default(),
            NodeMask: 0,
        };
        let command_queue: ID3D12CommandQueue =
            match unsafe { device.CreateCommandQueue(&queue_desc) } {
                Ok(queue) => queue,
                Err(hr) => panic!("Failed to create command queue: {:?}", hr),
            };

        Self {
            command_allocator,
            command_queue,
            command_list,
        }
    }
}

impl Display {
    fn new(
        factory: &IDXGIFactory7,
        device: &ID3D12Device,
        hwnd: &HWND,
        command_queue: &ID3D12CommandQueue,
    ) -> Self {
        let mut rect = RECT::default();
        unsafe {
            GetClientRect(*hwnd, &mut rect);
        }

        let swap_chain_desc = DXGI_SWAP_CHAIN_DESC1 {
            Width: (rect.right - rect.left) as u32,
            Height: (rect.bottom - rect.top) as u32,
            Format: DXGI_FORMAT_R8G8B8A8_UNORM,
            Stereo: false.into(),
            SampleDesc: DXGI_SAMPLE_DESC {
                Count: 1,
                Quality: 0,
            },
            BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
            BufferCount: Self::BACK_BUFFER_COUNT,
            Scaling: DXGI_SCALING_STRETCH,
            SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
            AlphaMode: DXGI_ALPHA_MODE_UNSPECIFIED,
            Flags: Default::default(),
        };

        let swap_chain: IDXGISwapChain4 = match unsafe {
            factory.CreateSwapChainForHwnd(command_queue, *hwnd, &swap_chain_desc, None, None)
        } {
            Ok(swap_chain) => swap_chain,
            Err(hr) => panic!("Failed to create swap chain: {:?}", hr),
        }
        .cast()
        .expect("Failed to cast swap chain to IDXGISwapChain4");

        let rtv_heap_desc = D3D12_DESCRIPTOR_HEAP_DESC {
            Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
            NumDescriptors: Self::BACK_BUFFER_COUNT,
            Flags: Default::default(),
            NodeMask: 0,
        };
        let rtv_heap: ID3D12DescriptorHeap =
            match unsafe { device.CreateDescriptorHeap(&rtv_heap_desc) } {
                Ok(heap) => heap,
                Err(hr) => panic!("Failed to create RTV descriptor heap: {:?}", hr),
            };

        let got_swap_chain_desc = match unsafe { swap_chain.GetDesc1() } {
            Ok(desc) => desc,
            Err(hr) => panic!("Failed to get swap chain description: {:?}", hr),
        };

        let back_buffers: Vec<ID3D12Resource> = (0..got_swap_chain_desc.BufferCount)
            .map(|i| match unsafe { swap_chain.GetBuffer(i) } {
                Ok(buffer) => buffer,
                Err(hr) => panic!("Failed to get back buffer {}: {:?}", i, hr),
            })
            .collect();

        back_buffers.iter().enumerate().for_each(|(i, buf)| {
            let mut rtv_handle = unsafe { rtv_heap.GetCPUDescriptorHandleForHeapStart() };
            let offset = unsafe {
                device.GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV) as usize * i
            };
            rtv_handle.ptr = rtv_handle.ptr.wrapping_add(offset);

            unsafe { device.CreateRenderTargetView(buf, None, rtv_handle) }
        });

        Self {
            swap_chain,
            back_buffers,
            rtv_heap,
        }
    }
}
