import {
  BoxGeometry,
  DepthFormat,
  DepthTexture,
  DirectionalLight,
  Mesh,
  MeshStandardMaterial,
  NearestFilter,
  OrthographicCamera,
  PerspectiveCamera,
  PlaneGeometry,
  Scene,
  ShaderMaterial,
  UnsignedShortType,
  WebGLRenderer,
  WebGLRenderTarget,
} from "three";
import WebGL from "three/addons/capabilities/WebGL.js";
import { OrbitControls } from "three/examples/jsm/Addons.js";

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

const geo2 = new BoxGeometry(1, 1, 1);
const mat2 = new MeshStandardMaterial({ color: 0xff0000 });
const cube2 = new Mesh(geo2, mat2);
cube2.position.x = 2;
cube2.position.y = 1;
cube2.position.z = 1;
scene.add(cube2);

const light = new DirectionalLight(0xffffff);
light.position.set(1, 1, 1);
light.intensity = 2;
scene.add(light);

const controls = new OrbitControls(camera, renderer.domElement);
controls.enableDamping = true;

const dpr = renderer.getPixelRatio();
const target = new WebGLRenderTarget(
  window.innerWidth * dpr,
  window.innerHeight * dpr
);
target.texture.minFilter = NearestFilter;
target.texture.magFilter = NearestFilter;
target.texture.generateMipmaps = false;
target.stencilBuffer = false;
target.samples = 0;

target.depthTexture = new DepthTexture(window.innerWidth, window.innerHeight);
target.depthTexture.type = UnsignedShortType;
target.depthTexture.format = DepthFormat;

const postCamera = new OrthographicCamera(-1, 1, 1, -1, 0, 1);
const postMaterial = new ShaderMaterial({
  vertexShader: `
    varying vec2 vUv;
    void main() {
      vUv = uv;
      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  fragmentShader: `
    #include <packing>

    varying vec2 vUv;
    uniform sampler2D tDiffuse;
    uniform sampler2D tDepth;
    uniform float cameraNear;
    uniform float cameraFar;


    float readDepth( sampler2D depthSampler, vec2 coord ) {
      float fragCoordZ = texture2D( depthSampler, coord ).x;
      float viewZ = perspectiveDepthToViewZ( fragCoordZ, cameraNear, cameraFar );
      return viewZToOrthographicDepth( viewZ, cameraNear, cameraFar );
    }

    void main() {
      //vec3 diffuse = texture2D( tDiffuse, vUv ).rgb;
      float depth = readDepth( tDepth, vUv );

      gl_FragColor.rgb = 1.0 - vec3( depth );
      gl_FragColor.a = 1.0;
    }
  `,
  uniforms: {
    cameraNear: { value: camera.near },
    cameraFar: { value: camera.far },
    tDiffuse: { value: null },
    tDepth: { value: null },
  },
});
const postPlane = new PlaneGeometry(2, 2);
const postMesh = new Mesh(postPlane, postMaterial);
const postScene = new Scene();
postScene.add(postMesh);

function animate() {
  requestAnimationFrame(animate);

  cube.rotation.x += 0.01;
  cube.rotation.y += 0.01;

  renderer.setRenderTarget(target);
  renderer.render(scene, camera);

  postMaterial.uniforms.tDiffuse.value = target.texture;
  postMaterial.uniforms.tDepth.value = target.depthTexture;

  renderer.setRenderTarget(null);
  renderer.render(postScene, postCamera);

  controls.update();
}

if (WebGL.isWebGL2Available()) {
  animate();
} else {
  const warning = WebGL.getWebGL2ErrorMessage();
  document.body.appendChild(warning);
}
