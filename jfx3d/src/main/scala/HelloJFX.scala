import scalafx.animation.RotateTransition
import scalafx.application.JFXApp3
import scalafx.scene.DepthTest.Enable
import scalafx.scene.{Group, PerspectiveCamera, PointLight, Scene}
import scalafx.scene.paint.{Color, PhongMaterial}
import scalafx.scene.shape.{MeshView, TriangleMesh}
import scalafx.scene.transform.Rotate

object HelloJFX extends JFXApp3 {
  override def start(): Unit = {
    val mesh = TriangleMesh()
    mesh.points = Array(
      0.0f, -0.5f, 0.0f,
      0.5f, 0.5f, 0.0f,
      -0.5f, 0.5f, 0.0f
    )
    mesh.texCoords = Array(
      0.0f, 0.0f
    )
    mesh.faces = Array(
      0, 0,
      1, 0,
      2, 0
    )

    val mat = new PhongMaterial {
      diffuseColor = Color.Red
      specularColor = Color.LightCoral
    }

    val meshView = new MeshView(mesh) {
      material = mat
    }

    val rotation = new RotateTransition {
      node = meshView
      byAngle = 360
      cycleCount = RotateTransition.Indefinite
      duration = scalafx.util.Duration(4000)
      axis = Rotate.YAxis
    }
    rotation.play()

    val cam = new PerspectiveCamera(true) {
      nearClip = 0.1
      farClip = 1000.0
      translateZ = -2.0
    }

    val light = new PointLight {
      color = Color.White
      translateX = 0
      translateY = 0
      translateZ = -1.0
    }

    val rootScene = new Group(meshView, light)
    rootScene.depthTest = Enable

    stage = new JFXApp3.PrimaryStage {
      title = "Hello JavaFX 3D"
      scene = new Scene(rootScene, 800, 600, true) {
        fill = Color.Gray
        camera = cam
      }
    }
  }
}
