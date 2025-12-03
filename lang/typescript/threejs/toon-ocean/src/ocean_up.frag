//
// Description : Array and textureless GLSL 2D/3D/4D simplex 
//               noise functions.
//      Author : Ian McEwan, Ashima Arts.
//  Maintainer : stegu
//     Lastmod : 20201014 (stegu)
//     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
//               Distributed under the MIT License. See LICENSE file.
//               https://github.com/ashima/webgl-noise
//               https://github.com/stegu/webgl-noise
// 

vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 mod289(vec4 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 permute(vec4 x) {
     return mod289(((x*34.0)+10.0)*x);
}

vec4 taylorInvSqrt(vec4 r)
{
  return 1.79284291400159 - 0.85373472095314 * r;
}

float snoise(vec3 v)
{ 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //   x0 = x0 - 0.0 + 0.0 * C.xxx;
  //   x1 = x0 - i1  + 1.0 * C.xxx;
  //   x2 = x0 - i2  + 2.0 * C.xxx;
  //   x3 = x0 - 1.0 + 3.0 * C.xxx;
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

// Permutations
  i = mod289(i); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.5 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 105.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
}

// --------------------------------------------------------------
float fbm_noise(vec3 inp, float detail, float roughness) {
    float lacunarity = 2.0;

    float fscale = 1.0;
    float amp = 1.0;
    float maxamp = 0.0;
    float sum = 0.0;

    for (int i = 0; i <= int(detail); i++) {
        float t = snoise(fscale * inp);
        sum += t * amp;
        maxamp += amp;
        amp *= roughness;
        fscale *= lacunarity;
    }

    float rmd = detail - floor(detail);
    if (rmd != 0.0) {
        float t = snoise(fscale * inp);
        float sum2 = sum + t * amp;
        return mix(0.5 * sum / maxamp + 0.5, 0.5 * sum2 / (maxamp + amp) + 0.5, rmd);
    } else {
        return 0.5 * sum / maxamp + 0.5;
    }
}

uniform float uTime;

varying vec3 vNormal;
varying vec3 vWorldPosition;
varying vec2 vUv;

vec2 random(vec2 p) {
    return fract(sin(vec2(dot(p, vec2(127.1, 311.7)), dot(p, vec2(269.5, 183.3)))) * 43758.5453 * 2.91238576);
}

#define VORONOI_SCALE 56.1

// voronoi node f1
float voronoi_distance(vec2 uv) {
    vec2 st = uv.xy * VORONOI_SCALE; 

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
    vec2 st = uv.xy * VORONOI_SCALE;

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

// voronoi node f2
vec2 voronoi_f1_f2_distance(vec2 uv) {
    vec2 st = uv.xy * VORONOI_SCALE;

    vec2 i_st = floor(st);
    vec2 f_st = fract(st);

    float f1_dist = 1.0;
    float f2_dist = 1.0;

    for (int y = -1; y <= 1; y++) {
        for (int x = -1; x <= 1; x++) {
            vec2 neighbor = vec2(float(x), float(y));
            
            vec2 point = random(i_st + neighbor);
            point = 0.5 + 0.5 * sin(uTime + point * 6.28318); // 2 * PI

            vec2 diff = neighbor + point - f_st;
            float dist = length(diff);
            if (dist < f1_dist) {
                f2_dist = f1_dist;
                f1_dist = dist;
            }
            else if (dist < f2_dist) {
                f2_dist = dist;
            }
        }
    }

    return vec2(f1_dist, f2_dist);
}

float wave(vec3 p) {
    p *= 0.002 * abs(vWorldPosition.y);
    float distortion = 50.0;
    float detail = 7.5;
    float detail_scale = 1.1;
    float detail_roughness = 0.538;
    float phase = 9.0;

    p = (p + 0.000001) * 0.999999;
    float n = length(p) * 20.0;
    n += phase;
    n += distortion * (fbm_noise(p, detail, detail_roughness) * 2.0 - 1.0);

    return 0.5 + 0.5 * sin(n - 6.28318);
}

float color_ramp_ease(float threshold1, float threshold2, float inp, float x, float y) {
    float t = smoothstep(threshold1, threshold2, inp);
    return mix(x, y, t);
}

void main() {
    float voronoi_f1_dist = voronoi_distance(vUv);
    float voronoi_f1_smooth_dist = voronoi_smooth_distance(vUv);

    float ramp_in = abs(voronoi_f1_dist - voronoi_f1_smooth_dist);
    float ramp_out = color_ramp_ease(0.15, 0.18, ramp_in, 0.0, 1.0);

    vec2 v = voronoi_f1_f2_distance(vUv);
    float d = v.y - v.x;
    ramp_out += 1.0 - smoothstep(0.0, 0.15, d);
    ramp_out = clamp(ramp_out, 0.0, 1.0);   

    float wave = wave(vWorldPosition);
    float wave_out = color_ramp_ease(0.991, 1.0, wave, 0.0, 1.0);

    float k = (1.0 - wave_out) * (1.0 - ramp_out);

    vec4 color = mix(vec4(0.8, 0.8, 0.8, 0.85), vec4(0.0, 0.0, 0.0, 1.0), k);

    gl_FragColor = color;
}