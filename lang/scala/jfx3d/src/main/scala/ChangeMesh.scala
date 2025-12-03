import scalafx.animation.{AnimationTimer, RotateTransition}
import scalafx.application.JFXApp3
import scalafx.scene.DepthTest.Enable
import scalafx.scene.{Group, PerspectiveCamera, PointLight, Scene}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape.CullFace.None
import scalafx.scene.shape.{MeshView, TriangleMesh}
import scalafx.scene.transform.Rotate

object ChangeMesh extends JFXApp3 {
  override def start(): Unit = {
    val mesh = TriangleMesh()
    mesh.points = Array(
      -1.0f, -1.0f, 0.0f,
      1.0f, -1.0f, 0.0f,
      -1.0f, 1.0f, 0.0f,
      1.0f, 1.0f, 0.0f
    )
    mesh.texCoords = Array(
      0.0f, 0.0f
    )
    mesh.faces = Array(
      0, 0,  1, 0,  2, 0,
      1, 0,  3, 0,  2, 0
    )

    val mat = new PhongMaterial {
      diffuseColor = Color.Red
      specularColor = Color.LightCoral
    }

    val meshView = new MeshView(mesh) {
      material = mat
      cullFace = None
    }

    val rotation = new RotateTransition {
      node = meshView
      byAngle = 360
      cycleCount = RotateTransition.Indefinite
      duration = scalafx.util.Duration(400000)
      axis = Rotate.YAxis
    }
    rotation.play()

    val cam = new PerspectiveCamera(true) {
      nearClip = 0.1
      farClip = 1000.0
      translateZ = -5.0
    }

    val light = new PointLight {
      color = Color.White
      translateX = 0
      translateY = 0
      translateZ = -3.0
    }

    val rootScene = new Group(meshView, light)
    rootScene.depthTest = Enable

    val startTime = System.nanoTime()
    val timer = AnimationTimer{ now =>
      val t = (now - startTime) / 1e9
      val newY = 1.0f + 0.5f * Math.sin(t).toFloat
      mesh.points.set(10, newY)
    }
    timer.start()

    stage = new JFXApp3.PrimaryStage {
      title = "Hello JavaFX 3D"
      scene = new Scene(rootScene, 800, 600, true) {
        fill = Color.Gray
        camera = cam
      }
    }
  }
}
