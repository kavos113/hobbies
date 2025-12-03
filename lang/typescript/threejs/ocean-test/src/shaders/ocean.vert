varying vec2 vUv;
varying float vElevation;

void main() {
    vUv = uv;
    
    vec4 modelPosition = modelMatrix * vec4(position, 1.0);
    
    // 波のアニメーション
    float elevation = sin(modelPosition.x * 3.0 + time) * 0.1
                   + sin(modelPosition.z * 2.0 + time * 0.8) * 0.15;
    
    modelPosition.y += elevation;
    vElevation = elevation;
    
    vec4 viewPosition = viewMatrix * modelPosition;
    vec4 projectedPosition = projectionMatrix * viewPosition;
    
    gl_Position = projectedPosition;
}
