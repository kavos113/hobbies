varying vec2 vUv;
varying float vElevation;

uniform vec3 uColorDeep;
uniform vec3 uColorSurface;

void main() {
    // 深さに基づいて色を混ぜる
    vec3 color = mix(uColorDeep, uColorSurface, vElevation * 2.0 + 0.5);
    
    // 簡単な光沢効果
    float fresnel = pow(1.0 + dot(vec3(0.0, 1.0, 0.0), normalize(vec3(0.0, vElevation, 1.0))), 3.0);
    color += vec3(0.5) * fresnel * 0.3;
    
    gl_FragColor = vec4(color, 1.0);
}
