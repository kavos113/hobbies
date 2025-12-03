import './style.css'
import * as THREE from 'three'
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls'

const vertexShader = /* glsl */`
varying vec2 vUv;
varying float vElevation;

uniform float time;

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
`

const fragmentShader = /* glsl */`
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
`

// シーンの設定
const scene = new THREE.Scene()
scene.fog = new THREE.FogExp2(0x1e4877, 0.0015)

// カメラの設定
const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000)
camera.position.set(0, 5, 10)

// レンダラーの設定
const renderer = new THREE.WebGLRenderer({ antialias: true })
renderer.setSize(window.innerWidth, window.innerHeight)
renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2))
renderer.setClearColor(0x1e4877)
document.body.appendChild(renderer.domElement)

// カメラコントロールの設定
const controls = new OrbitControls(camera, renderer.domElement)
controls.enableDamping = true
controls.dampingFactor = 0.05
controls.minDistance = 3
controls.maxDistance = 20
controls.maxPolarAngle = Math.PI * 0.45

// 海面のジオメトリ生成
const geometry = new THREE.PlaneGeometry(30, 30, 128, 128)
geometry.rotateX(-Math.PI / 2)

// シェーダーマテリアルの作成
const material = new THREE.ShaderMaterial({
    vertexShader,
    fragmentShader,
    uniforms: {
        time: { value: 0 },
        uColorDeep: { value: new THREE.Color(0x0d47a1) },    // 深い青
        uColorSurface: { value: new THREE.Color(0x64b5f6) }, // 明るい青
    },
})

// メッシュの作成
const ocean = new THREE.Mesh(geometry, material)
scene.add(ocean)

// 環境光の追加
const ambientLight = new THREE.AmbientLight(0xffffff, 0.5)
scene.add(ambientLight)

// 平行光源の追加
const directionalLight = new THREE.DirectionalLight(0xffffff, 1)
directionalLight.position.set(1, 1, 1)
scene.add(directionalLight)

// リサイズハンドラー
window.addEventListener('resize', () => {
    camera.aspect = window.innerWidth / window.innerHeight
    camera.updateProjectionMatrix()
    renderer.setSize(window.innerWidth, window.innerHeight)
})

// アニメーションループ
function animate() {
    requestAnimationFrame(animate)
    
    // 時間の更新
    material.uniforms.time.value = performance.now() * 0.001
    
    controls.update()
    renderer.render(scene, camera)
}

animate()
