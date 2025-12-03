import {
  BoxGeometry,
  Clock,
  DirectionalLight,
  Mesh,
  MeshStandardMaterial,
  PerspectiveCamera,
  PlaneGeometry,
  RepeatWrapping,
  Scene,
  TextureLoader,
  WebGLRenderer,
} from "three";
import WebGL from "three/addons/capabilities/WebGL.js";
import { Refractor, WaterRefractionShader } from "three/examples/jsm/Addons.js";

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
scene.add(cube);

const light = new DirectionalLight(0xffffff);
light.position.set(1, 1, 1);
light.intensity = 2;
scene.add(light);

const clock = new Clock();

const refg = new PlaneGeometry(10, 10);
const refractor = new Refractor(refg, {
  color: 0x00ffff,
  textureWidth: 512,
  textureHeight: 512,
  shader: WaterRefractionShader,
});
scene.add(refractor);

const dudv = new TextureLoader().load("./waterdudv.jpg");
dudv.wrapS = RepeatWrapping;
dudv.wrapT = RepeatWrapping;
refractor.material.uniforms.tDudv.value = dudv;

function animate() {
  requestAnimationFrame(animate);

  cube.rotation.x += 0.01;
  cube.rotation.y += 0.01;

  renderer.render(scene, camera);
}

if (WebGL.isWebGL2Available()) {
  animate();
} else {
  const warning = WebGL.getWebGL2ErrorMessage();
  document.body.appendChild(warning);
}
