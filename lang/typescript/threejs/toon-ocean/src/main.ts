import {
  AdditiveBlending,
  BoxGeometry,
  Clock,
  Color,
  DirectionalLight,
  DoubleSide,
  Mesh,
  MeshStandardMaterial,
  PerspectiveCamera,
  PlaneGeometry,
  Scene,
  ShaderMaterial,
  Vector2,
  Vector3,
  WebGLRenderer,
} from "three";
import WebGL from "three/addons/capabilities/WebGL.js";
import { OrbitControls } from "three/examples/jsm/Addons.js";
import fs from "./ocean.frag?raw";
import vs from "./ocean.vert?raw";
import fsUp from "./ocean_up.frag?raw";
import vsBase from "./ocean_base.vert?raw";

const scene = new Scene();
const camera = new PerspectiveCamera(
  90,
  window.innerWidth / window.innerHeight,
  0.1,
  1000
);
camera.position.z = 5;

const renderer = new WebGLRenderer();
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

const geometry = new BoxGeometry(1, 1, 1);
const material = new MeshStandardMaterial({ color: 0x00ffff });
const cube = new Mesh(geometry, material);
// scene.add(cube);

const light = new DirectionalLight(0xffffff);
light.position.set(1, 1, 1);
light.intensity = 2;
scene.add(light);

const ambientLight = new DirectionalLight(0xffffff, 1.5);
ambientLight.position.set(0, 0, 1);
scene.add(ambientLight);

const controls = new OrbitControls(camera, renderer.domElement);
controls.minDistance = 1;
controls.maxDistance = 100;

// ocean
const oceanGeometry = new PlaneGeometry(100, 100, 100, 100);
const uniforms = {
  uTime: {
    value: 0,
  },
  uWaterColor: {
    value: new Color(0x90c1ef),
  },
  uShadowColor: {
    value: new Color(0x5576c3),
  },
};
const oceanMaterial = new ShaderMaterial({
  uniforms: uniforms,
  vertexShader: vsBase,
  fragmentShader: fs,
  side: DoubleSide,
});
const oceanMesh = new Mesh(oceanGeometry, oceanMaterial);
oceanMesh.rotation.x = Math.PI / 2;
oceanMesh.position.y = -2;
scene.add(oceanMesh);

const oceanUpGeometry = new PlaneGeometry(100, 100, 100, 100);
const uniformsUp = {
  uTime: {
    value: 0,
  },
};
const oceanMaterialUp = new ShaderMaterial({
  uniforms: uniformsUp,
  vertexShader: vs,
  fragmentShader: fsUp,
  side: DoubleSide,
  transparent: true,
  depthWrite: false,
  depthTest: false,
  blending: AdditiveBlending,
});
const oceanMeshUp = new Mesh(oceanUpGeometry, oceanMaterialUp);
oceanMeshUp.rotation.x = Math.PI / 2;
oceanMeshUp.position.y = -1;
scene.add(oceanMeshUp);

const clock = new Clock();

function animate() {
  requestAnimationFrame(animate);

  const elapsedTime = clock.getElapsedTime();
  uniforms.uTime.value = elapsedTime;
  uniformsUp.uTime.value = elapsedTime;

  renderer.render(scene, camera);
}

if (WebGL.isWebGL2Available()) {
  animate();
} else {
  const warning = WebGL.getWebGL2ErrorMessage();
  document.body.appendChild(warning);
}
