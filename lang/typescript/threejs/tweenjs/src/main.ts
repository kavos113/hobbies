import * as THREE from "three";
import * as TWEEN from "@tweenjs/tween.js";

// シーンの作成
const scene = new THREE.Scene();

// カメラの作成
const camera = new THREE.PerspectiveCamera(
  75,
  window.innerWidth / window.innerHeight,
  0.1,
  1000
);
camera.position.z = 20;

// レンダラーの作成
const renderer = new THREE.WebGLRenderer();
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

// 動くターゲット（球体）の作成
const geometry = new THREE.SphereGeometry(1, 32, 32);
const material = new THREE.MeshPhongMaterial({ color: 0xff0000 });
const target = new THREE.Mesh(geometry, material);
scene.add(target);

// 光源の追加
const light = new THREE.DirectionalLight(0xffffff, 1);
light.position.set(1, 1, 1);
scene.add(light);
const ambientLight = new THREE.AmbientLight(0x404040);
scene.add(ambientLight);

// ターゲットの移動アニメーション
const targetTween = new TWEEN.Tween(target.position)
  .to({ x: 5, y: 3, z: 0 }, 2000)
  .repeat(Infinity)
  .yoyo(true)
  .easing(TWEEN.Easing.Quadratic.InOut)
  .start();

const startPosition = {
  x: camera.position.x,
  y: camera.position.y,
  z: camera.position.z,
};

const cameraZoomIn = new TWEEN.Tween(startPosition)
  .to({ z: 5 }, 1500)
  .easing(TWEEN.Easing.Quadratic.InOut)
  .onUpdate(zoomInUpdate);

function zoomInUpdate() {
  camera.position.x =
    startPosition.x + target.position.x * (1 - startPosition.z / 20);
  camera.position.y =
    startPosition.y + target.position.y * (1 - startPosition.z / 20);
  camera.position.z = startPosition.z;
}

// カメラのズームインアニメーション
let isZooming = false;
document.addEventListener("click", () => {
  if (isZooming) return;
  isZooming = true;
  cameraZoomIn.start();
});

const group = new TWEEN.Group(targetTween, cameraZoomIn);

// アニメーションループ
function animate() {
  requestAnimationFrame(animate);
  group.update();
  renderer.render(scene, camera);
}

// ウィンドウリサイズ対応
window.addEventListener("resize", () => {
  const width = window.innerWidth;
  const height = window.innerHeight;
  renderer.setSize(width, height);
  camera.aspect = width / height;
  camera.updateProjectionMatrix();
});

animate();
