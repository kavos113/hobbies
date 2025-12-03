import * as THREE from 'three';
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';

class GLBViewer {
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private renderer: THREE.WebGLRenderer;
  private controls: OrbitControls;
  private loader: GLTFLoader;

  constructor() {
    // シーンの作成
    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0x2a2a2a);

    // カメラの設定
    this.camera = new THREE.PerspectiveCamera(
      75,
      window.innerWidth / window.innerHeight,
      0.1,
      1000
    );
    this.camera.position.z = 5;

    // レンダラーの設定
    this.renderer = new THREE.WebGLRenderer({ antialias: true });
    this.renderer.setSize(window.innerWidth, window.innerHeight);
    this.renderer.setPixelRatio(window.devicePixelRatio);
    document.body.appendChild(this.renderer.domElement);

    // コントロールの設定
    this.controls = new OrbitControls(this.camera, this.renderer.domElement);
    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.05;

    // ライティングの設定
    const ambientLight = new THREE.AmbientLight(0xffffff, 0.5);
    this.scene.add(ambientLight);

    const directionalLight = new THREE.DirectionalLight(0xffffff, 1);
    directionalLight.position.set(5, 5, 5);
    this.scene.add(directionalLight);

    // GLTFローダーの初期化
    this.loader = new GLTFLoader();

    // リサイズイベントの設定
    window.addEventListener('resize', this.onWindowResize.bind(this));

    // ファイルドロップの設定
    this.setupFileDrop();

    // アニメーションの開始
    this.animate();
  }

  private onWindowResize(): void {
    this.camera.aspect = window.innerWidth / window.innerHeight;
    this.camera.updateProjectionMatrix();
    this.renderer.setSize(window.innerWidth, window.innerHeight);
  }

  private dropZone!: HTMLDivElement;

  private setupFileDrop(): void {
    this.dropZone = document.createElement('div');
    this.dropZone.className = 'drop-zone';
    this.dropZone.textContent = 'GLBファイルをドロップしてください';
    document.body.appendChild(this.dropZone);

    document.body.addEventListener('dragover', (event) => {
      event.preventDefault();
      event.stopPropagation();
      this.dropZone.classList.add('drag-over');
    });

    document.body.addEventListener('dragleave', (event) => {
      event.preventDefault();
      event.stopPropagation();
      this.dropZone.classList.remove('drag-over');
    });

    document.body.addEventListener('drop', (event) => {
      event.preventDefault();
      event.stopPropagation();
      this.dropZone.classList.remove('drag-over');

      const file = event.dataTransfer?.files[0];
      if (file && file.name.toLowerCase().endsWith('.glb')) {
        this.loadGLBFile(file);
        this.dropZone.style.display = 'none';
      }
    });
  }

  private loadGLBFile(file: File): void {
    const reader = new FileReader();
    reader.onload = (event) => {
      const arrayBuffer = event.target?.result;
      if (arrayBuffer instanceof ArrayBuffer) {
        // 既存のモデルを削除
        this.scene.children = this.scene.children.filter(
          child => child instanceof THREE.Light
        );

        this.loader.parse(arrayBuffer, '', (gltf) => {
          const model = gltf.scene;
          
          // モデルのバウンディングボックスを計算
          const box = new THREE.Box3().setFromObject(model);
          const center = box.getCenter(new THREE.Vector3());
          const size = box.getSize(new THREE.Vector3());
          
          // モデルを中心に配置
          model.position.sub(center);
          
          // カメラの位置を調整
          const maxDim = Math.max(size.x, size.y, size.z);
          this.camera.position.z = maxDim * 2;
          
          this.scene.add(model);
        }, (error) => {
          console.error('GLBファイルの読み込みエラー:', error);
        });
      }
    };
    reader.readAsArrayBuffer(file);
  }

  private animate(): void {
    requestAnimationFrame(() => this.animate());
    this.controls.update();
    this.renderer.render(this.scene, this.camera);
  }
}

// スタイルの適用
const style = document.createElement('style');
style.textContent = `
  body { 
    margin: 0;
    overflow: hidden;
  }
  .drop-zone {
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    padding: 20px;
    background-color: rgba(255, 255, 255, 0.8);
    border: 2px dashed #666;
    border-radius: 8px;
    pointer-events: none;
    transition: opacity 0.3s ease;
  }
  .drag-over .drop-zone {
    background-color: rgba(255, 255, 255, 0.9);
    border-color: #333;
  }
`;
document.head.appendChild(style);

// ビューワーの初期化
new GLBViewer();
