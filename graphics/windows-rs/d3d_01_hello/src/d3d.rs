mod device;
mod resource;

use crate::d3d::device::Device;
use crate::d3d::resource::Resources;
use crate::Renderer;
use windows::Win32::Foundation::HWND;

pub struct D3DRenderer {
    device: Device,
    resources: Resources,
}

impl Renderer for D3DRenderer {
    fn new() -> Self
    where
        Self: Sized,
    {
        todo!()
    }

    fn bind_window(&mut self, hwnd: &HWND) {
        todo!()
    }

    fn render(&mut self) {
        todo!()
    }
}
