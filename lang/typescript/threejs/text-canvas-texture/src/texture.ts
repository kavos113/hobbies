import { CanvasTexture, LinearFilter, RGBAFormat } from "three";

export function createTexture(): CanvasTexture {
    const canvas = document.createElement('canvas');
    const c = canvas.getContext('2d');
    if (!c) {
        throw new Error('Failed to get 2d context');
    }
    
    const context = c;
    
    canvas.width = 256;
    canvas.height = 256;
    
    context.fillStyle = 'white';
    context.fillRect(0, 0, 256, 256);
    
    context.font = '120px Arial';
    context.textAlign = 'center';
    context.textBaseline = 'middle';
    context.fillStyle = 'black';
    context.fillText('Hello, World!', canvas.width / 2, canvas.height / 2);
    
    const texture = new CanvasTexture(canvas)
    texture.needsUpdate = true;
    texture.minFilter = LinearFilter;
    texture.magFilter = LinearFilter;
    texture.format = RGBAFormat;
    
    return texture;
}