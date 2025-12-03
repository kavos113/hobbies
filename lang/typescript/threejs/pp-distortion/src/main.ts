import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls.js";
import { EffectComposer } from "three/examples/jsm/postprocessing/EffectComposer.js";
import { RenderPass } from "three/examples/jsm/postprocessing/RenderPass.js";
import { ShaderPass } from "three/examples/jsm/postprocessing/ShaderPass.js";

let scene, camera, renderer, composer, controls, distortionPass;
let clock = new THREE.Clock();

// --- 1. 初期化 ---
function init() {
  // --- 2. 基本的なThree.jsセットアップ ---
  scene = new THREE.Scene();
  camera = new THREE.PerspectiveCamera(
    75,
    window.innerWidth / window.innerHeight,
    0.1,
    1000
  );
  camera.position.set(0, 5, 10); // カメラ位置を調整

  renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setSize(window.innerWidth, window.innerHeight);
  document.body.appendChild(renderer.domElement);

  controls = new OrbitControls(camera, renderer.domElement);

  // ライト
  const ambientLight = new THREE.AmbientLight(0xffffff, 1.55);
  scene.add(ambientLight);
  const directionalLight = new THREE.DirectionalLight(0xffffff, 0.8);
  directionalLight.position.set(5, 10, 7.5);
  scene.add(directionalLight);

  // オブジェクト (水面下にある想定)
  const geometry = new THREE.TorusKnotGeometry(1.5, 0.5, 100, 16);
  const material = new THREE.MeshStandardMaterial({
    color: 0x00ffcc,
    roughness: 0.3,
    metalness: 0.5,
  });
  const torusKnot = new THREE.Mesh(geometry, material);
  torusKnot.position.y = 0; // 水面より下あたりに配置
  scene.add(torusKnot);

  // 水面 (視覚的な目安として)
  const waterGeometry = new THREE.PlaneGeometry(20, 20);
  const waterMaterial = new THREE.MeshStandardMaterial({
    color: 0x0077ff,
    transparent: true,
    opacity: 0.4,
    roughness: 0.1,
    metalness: 0.2,
  });
  const waterPlane = new THREE.Mesh(waterGeometry, waterMaterial);
  waterPlane.rotation.x = -Math.PI / 2;
  waterPlane.position.y = 2.0; // 水面のY座標
  scene.add(waterPlane);

  // --- 3. 法線マップテクスチャの読み込み ---
  const textureLoader = new THREE.TextureLoader();
  const normalMapTexture = textureLoader.load(
    "./waternormals.jpg", // あなたの法線マップ画像のパスを指定
    (texture) => {
      texture.wrapS = texture.wrapT = THREE.RepeatWrapping; // テクスチャをリピートさせる
      console.log("Normal map loaded.");
      // テクスチャがロードされたらパスをセットアップ
      setupPostprocessing(texture);
      // アニメーション開始
      animate();
    },
    undefined, // onProgress (省略)
    (err) => {
      console.error("Error loading normal map texture:", err);
      // エラー時も最低限動くように、空のテクスチャでセットアップ
      setupPostprocessing(new THREE.Texture());
      animate();
    }
  );

  // ウィンドウリサイズ対応
  window.addEventListener("resize", onWindowResize);
}

// --- 4 & 6. ポストプロセス設定 ---
function setupPostprocessing(normalMap) {
  // --- 4. EffectComposerの初期化とRenderPassの追加 ---
  composer = new EffectComposer(renderer);
  const renderPass = new RenderPass(scene, camera);
  composer.addPass(renderPass);

  // --- 5. 歪みエフェクト用カスタムシェーダーの定義 ---
  const DistortionShader = {
    uniforms: {
      tDiffuse: { value: null }, // EffectComposerが自動的にセット
      tNormalMap: { value: normalMap },
      time: { value: 0.0 },
      distortionScale: { value: 0.05 }, // 歪みの強さ
      screenResolution: {
        value: new THREE.Vector2(window.innerWidth, window.innerHeight),
      },
      waterLevelScreenY: { value: 0.5 }, // 水面のスクリーンY座標 (0.0=下, 1.0=上) - 後で計算
    },

    vertexShader: /* glsl */ `
            varying vec2 vUv;
            void main() {
                vUv = uv;
                gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
            }`,

    fragmentShader: /* glsl */ `
            uniform sampler2D tDiffuse;
            uniform sampler2D tNormalMap;
            uniform float time;
            uniform float distortionScale;
            uniform vec2 screenResolution; // 解像度を追加（必要なら）
            uniform float waterLevelScreenY; // 水面のスクリーン座標Y
            varying vec2 vUv;

            void main() {
                vec2 distortedUv = vUv;
                float distortionAmount = 0.0;

                // 水面より下 (スクリーン座標で判定 - 近似)
                if (vUv.y < waterLevelScreenY) {
                    // 法線マップを時間で動かす
                    vec2 normalUv = vUv * vec2(2.0, 2.0) + vec2(time * 0.01, time * 0.02); // UVスケールと時間によるオフセット

                    // 法線マップから法線ベクトルを取得 (-1 ~ 1の範囲に)
                    vec3 normalSample = texture2D(tNormalMap, normalUv).rgb * 2.0 - 1.0;

                    // 法線のX, Y成分を使ってUVを歪ませる量を計算
                    // vUv.y が waterLevelScreenY に近いほど歪みを弱くする（境界を滑らかに）
                    float fade = smoothstep(waterLevelScreenY - 0.1, waterLevelScreenY, vUv.y); // 0.1はフェード範囲
                    distortionAmount = (1.0 - fade) * distortionScale;

                    // XY方向の歪み（視差も考慮するとよりリアルだが、ここでは単純化）
                    distortedUv += normalSample.xy * distortionAmount;
                }

                // 歪んだUV座標で元のシーンテクスチャをサンプリング
                vec4 color = texture2D(tDiffuse, distortedUv);
                gl_FragColor = color;
            }`,
  };

  // --- 6. ShaderPassの作成とEffectComposerへの追加 ---
  distortionPass = new ShaderPass(DistortionShader);
  // distortionPass.renderToScreen = true; // 通常、最後のパスにこれを設定するが、今回はEffectComposerが自動で処理
  composer.addPass(distortionPass);
}

// --- 水面のY座標をスクリーン座標に変換するヘルパー ---
function updateWaterLevelScreenY() {
  if (!distortionPass) return;

  const waterWorldY = 2.0; // 水面のワールドY座標 (initで設定したものと同じ)
  const waterWorldPos = new THREE.Vector3(0, waterWorldY, 0);
  const waterScreenPos = waterWorldPos.project(camera); // ワールド座標 -> NDC (-1 to 1)
  const waterScreenY = (waterScreenPos.y + 1.0) / 2.0; // NDC -> Screen UV (0 to 1)

  distortionPass.uniforms.waterLevelScreenY.value = waterScreenY;
}

// --- 7. アニメーションループ ---
function animate() {
  requestAnimationFrame(animate);

  const delta = clock.getDelta();
  controls.update();

  // シェーダーのuniformを更新
  if (distortionPass) {
    distortionPass.uniforms.time.value += delta;
    updateWaterLevelScreenY(); // カメラが変わる可能性があるので毎フレーム更新
  }

  // レンダラーではなく、EffectComposerでレンダリング
  if (composer) {
    composer.render(delta);
  } else {
    renderer.render(scene, camera); // フォールバック
  }
}

// --- ウィンドウリサイズ処理 ---
function onWindowResize() {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
  if (composer) {
    composer.setSize(window.innerWidth, window.innerHeight);
  }
  if (distortionPass) {
    distortionPass.uniforms.screenResolution.value.set(
      window.innerWidth,
      window.innerHeight
    );
  }
}

// --- 開始 ---
init();
