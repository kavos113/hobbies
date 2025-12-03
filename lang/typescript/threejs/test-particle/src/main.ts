import * as THREE from "three";

const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera(
  75,
  window.innerWidth / window.innerHeight,
  0.1,
  1000
);
camera.position.set(0, 5, 10);
camera.lookAt(new THREE.Vector3(0, 0, 0));

const renderer = new THREE.WebGLRenderer();
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

// パーティクル数と属性の設定
const particleCount = 30000;
const positions = new Float32Array(particleCount * 3);
const speeds = new Float32Array(particleCount);

for (let i = 0; i < particleCount; i++) {
  // x座標と z座標はランダム配置、yは地面（0）に固定
  positions[i * 3] = (Math.random() - 0.5) * 10;
  positions[i * 3 + 1] = 0;
  positions[i * 3 + 2] = (Math.random() - 0.5) * 10;
  // パーティクルごとの上昇速度を設定（0.5 ~ 1.5）
  speeds[i] = 0.5 + Math.random();
}

const geometry = new THREE.BufferGeometry();
geometry.setAttribute("position", new THREE.BufferAttribute(positions, 3));

const material = new THREE.PointsMaterial({ color: 0xffffff, size: 0.02, transparent: true, opacity: 1 });
const particles = new THREE.Points(geometry, material);
scene.add(particles);

window.addEventListener("resize", onWindowResize, false);
function onWindowResize() {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
}

const clock = new THREE.Clock();
function animate() {
  requestAnimationFrame(animate);
  const delta = clock.getDelta();
  const positionsArray = geometry.attributes.position.array as Float32Array;
  for (let i = 0; i < particleCount; i++) {
    positionsArray[i * 3 + 1] += speeds[i] * delta;
    // y座標が一定の高さに達したらリセット
    if (positionsArray[i * 3 + 1] > 10) {
      positionsArray[i * 3] = (Math.random() - 0.5) * 10;
      positionsArray[i * 3 + 1] = 0;
      positionsArray[i * 3 + 2] = (Math.random() - 0.5) * 10;
      speeds[i] = 0.5 + Math.random();
    }
  }
  geometry.attributes.position.needsUpdate = true;
  material.opacity = (Math.sin(clock.elapsedTime * 2) + 1) / 2;
  renderer.render(scene, camera);
}

animate();
