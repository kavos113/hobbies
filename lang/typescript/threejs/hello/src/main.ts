import * as THREE from 'three';
import WebGL from 'three/addons/capabilities/WebGL.js';

const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera(90, window.innerWidth / window.innerHeight, 0.1, 1000);
camera.position.z = 5;

const renderer = new THREE.WebGLRenderer();
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

const geometry = new THREE.BoxGeometry(1, 1, 1);
const material = new THREE.MeshStandardMaterial({color: 0x00ffff});
const cube = new THREE.Mesh(geometry, material);
//scene.add(cube);

const light = new THREE.DirectionalLight(0xffffff);
light.position.set(1, 1, 1);
light.intensity = 2;
scene.add(light);

const mat = new THREE.ShaderMaterial({
  vertexShader: `
    void main() {
      vec4 worldPosition = modelMatrix * vec4(position, 1.0);
      vec4 viewPosition = viewMatrix * worldPosition;
      gl_Position = projectionMatrix * viewPosition;
    }
  `,
  fragmentShader: `
    void main() {
      gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
    }
  `
});
const cube2 = new THREE.Mesh(geometry, mat);
scene.add(cube2);
cube2.position.x = 2;

const context = effekseer.createContext();
context.init(renderer.getContext());

const fastRenderMode = true;
if (fastRenderMode) {
  context.setRestorationOfStatesFlag(false);
}

const effect = context.loadEffect("Laser01.efkefc", 1.0, () => {
  const handle = context.play(effect, 0, 0, 0);
  handle.setLocation(0, 0, 0);
});

function animate() {
  requestAnimationFrame(animate);

  cube.rotation.x += 0.1;
  cube.rotation.y += 0.1;

  renderer.render(scene, camera);
}

if (WebGL.isWebGL2Available()) {
  animate();
} else {
  const warning = WebGL.getWebGL2ErrorMessage();
  document.body.appendChild(warning);
}
