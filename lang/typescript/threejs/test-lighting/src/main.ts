import {
  BoxGeometry,
  DirectionalLight,
  Mesh,
  MeshStandardMaterial,
  PerspectiveCamera,
  Scene,
  WebGLRenderer,
  AmbientLight,
  AnimationMixer,
  AnimationClip,
} from "three";
import WebGL from "three/addons/capabilities/WebGL.js";
import { GLTFLoader } from "three/examples/jsm/loaders/GLTFLoader.js";

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
renderer.setClearColor(0xffffff);

// 環境光を追加して暗い部分も見えるようにする
const ambientLight = new AmbientLight(0xffffff, 0.5);
scene.add(ambientLight);

let mixer: AnimationMixer;

// GLTFLoaderのインポートパスを修正し、エラーハンドリングを追加
const loader = new GLTFLoader();
loader.load(
  "../assets/ship_ver3.glb",
  (gltf) => {
    console.log("Model loaded successfully:", gltf);
    const model = gltf.scene;

    model.scale.set(1, 1, 1);
    model.position.set(0, 0, 0);
    scene.add(model);
    camera.position.z = 5;

    const clips = gltf.animations;
    const clip = AnimationClip.findByName(clips, "Action");

    mixer = new AnimationMixer(model);
    mixer.clipAction(clip).play();
  },
  (xhr) => {
    console.log((xhr.loaded / xhr.total) * 100 + "% loaded");
  },
  (error) => {
    console.error("エラーが発生しました:", error);
  }
);

const light = new DirectionalLight(0xffffff);
light.position.set(1, 1, 1);
light.intensity = 2;
scene.add(light);

window.addEventListener("resize", () => {
  const width = window.innerWidth;
  const height = window.innerHeight;

  camera.aspect = width / height;
  camera.updateProjectionMatrix();

  renderer.setSize(width, height);
});

function animate() {
  requestAnimationFrame(animate);

  renderer.render(scene, camera);

  if (mixer) {
    mixer.update(0.01);
  }
}

if (WebGL.isWebGL2Available()) {
  animate();
} else {
  const warning = WebGL.getWebGL2ErrorMessage();
  document.body.appendChild(warning);
}
