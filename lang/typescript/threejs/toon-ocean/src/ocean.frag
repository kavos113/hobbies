uniform vec3 uWaterColor;
uniform vec3 uShadowColor;
uniform float uTime;

varying vec3 vNormal;
varying vec3 vWorldPosition;
varying vec2 vUv;

vec2 random(vec2 p) {
    return fract(sin(vec2(dot(p, vec2(127.1, 311.7)), dot(p, vec2(269.5, 183.3)))) * 43758.5453);
}

// voronoi node f1
float voronoi_distance(vec2 uv) {
    vec2 st = uv.xy * 20.0; 

    // tiling
    vec2 i_st = floor(st);
    vec2 f_st = fract(st);

    float min_dist = 1.0;

    for (int y = -1; y <= 1; y++) {
        for (int x = -1; x <= 1; x++) {
            vec2 neighbor = vec2(float(x), float(y));

            vec2 point = random(i_st + neighbor);
            point = 0.5 + 0.5 * sin(uTime + point * 6.28318); // 2 * PI

            vec2 diff = neighbor + point - f_st;
            float dist = length(diff);
            min_dist = min(min_dist, dist);
        }
    }

    return min_dist;
}

// voronoi node f1(smooth)
float voronoi_smooth_distance(vec2 uv) {
    vec2 st = uv.xy * 20.0;

    // tiling
    vec2 i_st = floor(st);
    vec2 f_st = fract(st);

    float smooth_dist = 0.0;
    float h = -1.0;

    for (int y = -2; y <= 2; y++) {
        for (int x = -2; x <= 2; x++) {
            vec2 neighbor = vec2(float(x), float(y));

            vec2 point = random(i_st + neighbor);
            point = 0.5 + 0.5 * sin(uTime + point * 6.28318); // 2 * PI

            vec2 diff = neighbor + point - f_st;
            float dist = length(diff);

            float smoothness = 0.6;
            h = h == -1.0 ? 1.0 : smoothstep(0.0, 1.0, 0.5 + 0.5 * (smooth_dist - dist) / smoothness);
            float correction_factor = smoothness * h * (1.0 - h);

            smooth_dist = mix(smooth_dist, dist, h) - correction_factor;
        }
    }

    return smooth_dist;
}

void main() {
    float voronoi_f1_dist = voronoi_distance(vUv);
    float voronoi_f1_smooth_dist = voronoi_smooth_distance(vUv);

    float ramp_in = abs(voronoi_f1_dist - voronoi_f1_smooth_dist);

    // color ramp
    float threshold1 = 0.095;
    float threshold2 = 0.177;

    float t = smoothstep(threshold1, threshold2, ramp_in);
    vec3 ramp_color = mix(uWaterColor, uShadowColor, t);

    gl_FragColor = vec4(ramp_color, 1.0);
}