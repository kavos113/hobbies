import * as THREE from "three";
import WebGL from "three/addons/capabilities/WebGL.js";

const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera(
  90,
  window.innerWidth / window.innerHeight,
  0.1,
  1000
);
camera.position.z = 20; // カメラを少し後ろに下げる

const renderer = new THREE.WebGLRenderer();
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

const light = new THREE.DirectionalLight(0xffffff);
light.position.set(1, 1, 1);
light.intensity = 2;
scene.add(light);

// 通常のキューブは残しておく
const geometry = new THREE.BoxGeometry(1, 1, 1);
const material = new THREE.MeshStandardMaterial({ color: 0x00ffff });
const cube = new THREE.Mesh(geometry, material);
scene.add(cube);

// instancing
// インスタンスの数を定義
const instanceCount = 1000;

// インスタンシング用のマテリアル
const instancedMaterial = new THREE.MeshPhongMaterial({
  color: 0xffff00,
  emissive: 0x072534,
  side: THREE.DoubleSide,
  flatShading: true,
});

// インスタンシングメッシュを作成
const instancedMesh = new THREE.InstancedMesh(
  new THREE.BoxGeometry(0.5, 0.5, 0.5),
  instancedMaterial,
  instanceCount
);

// 各インスタンスの位置、回転、スケールを設定
const dummy = new THREE.Object3D();
const range = 10; // インスタンスが配置される範囲

for (let i = 0; i < instanceCount; i++) {
  // ランダムな位置を設定
  dummy.position.set(
    (Math.random() - 0.5) * range,
    (Math.random() - 0.5) * range,
    (Math.random() - 0.5) * range
  );

  // ランダムな回転を設定
  dummy.rotation.set(
    Math.random() * Math.PI,
    Math.random() * Math.PI,
    Math.random() * Math.PI
  );

  // ランダムなスケールを設定
  const scale = 0.1 + Math.random() * 0.9;
  dummy.scale.set(scale, scale, scale);

  // 行列を更新
  dummy.updateMatrix();

  // インスタンスに行列を適用
  instancedMesh.setMatrixAt(i, dummy.matrix);

  // ランダムな色を設定
  instancedMesh.setColorAt(i, new THREE.Color(Math.random() * 0xffffff));
}

// インスタンスバッファを更新
instancedMesh.instanceMatrix.needsUpdate = true;
if (instancedMesh.instanceColor) instancedMesh.instanceColor.needsUpdate = true;

// シーンに追加
scene.add(instancedMesh);

function animate() {
  requestAnimationFrame(animate);

  // 元のキューブの回転
  cube.rotation.x += 0.01;
  cube.rotation.y += 0.01;

  // インスタンシングメッシュ全体の回転
  instancedMesh.rotation.x += 0.003;
  instancedMesh.rotation.y += 0.005;

  // 時間に基づいて各インスタンスの位置を少し変更することも可能
  const time = Date.now() * 0.001;

  // すべてのインスタンスをアニメーションさせるには重いので
  // 一部のインスタンスだけをサンプルとして動かす
  for (let i = 0; i < 10; i++) {
    dummy.position.set(
      Math.sin(time + i) * 0.5 + (Math.random() - 0.5) * 0.2,
      Math.cos(time + i * 0.5) * 0.5 + (Math.random() - 0.5) * 0.2,
      Math.sin(time + i * 0.3) * 0.5 + (Math.random() - 0.5) * 0.2
    );
    dummy.updateMatrix();
    instancedMesh.setMatrixAt(i, dummy.matrix);
  }

  // バッファを更新
  instancedMesh.instanceMatrix.needsUpdate = true;

  renderer.render(scene, camera);
}

if (WebGL.isWebGL2Available()) {
  animate();
} else {
  const warning = WebGL.getWebGL2ErrorMessage();
  document.body.appendChild(warning);
}
