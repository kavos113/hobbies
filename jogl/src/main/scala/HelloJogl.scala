import com.jogamp.opengl.{GL, GL2, GLAutoDrawable, GLCapabilities, GLEventListener, GLProfile}
import com.jogamp.opengl.awt.GLCanvas
import com.jogamp.opengl.glu.GLU
import com.jogamp.opengl.util.FPSAnimator

import java.awt.Dimension
import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing.JFrame

object HelloJogl {
  @main def main(): Unit = {
    val profile = GLProfile.get(GLProfile.GL2)
    val capabilities = GLCapabilities(profile)
    capabilities.setDoubleBuffered(true)

    val canvas = GLCanvas(capabilities)
    canvas.addGLEventListener(Renderer())
    canvas.setPreferredSize(Dimension(800, 600))

    val animator = FPSAnimator(canvas, 60)

    val frame = JFrame("sample")
    frame.getContentPane.add(canvas)
    frame.setSize(frame.getContentPane.getPreferredSize)
    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        if (animator.isStarted) animator.stop()
        System.exit(0)
      }
    })

    frame.setVisible(true)
    animator.start()
  }
}

class Renderer extends GLEventListener {
  private var angle = 0.0f
  private val glu = GLU()

  override def init(drawable: GLAutoDrawable): Unit = {
    val gl = drawable.getGL.getGL2
    gl.glClearColor(0f, 0f, 0f, 1f)
    gl.glEnable(GL.GL_DEPTH_TEST)
  }

  override def display(drawable: GLAutoDrawable): Unit = {
    val gl = drawable.getGL.getGL2
    gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT)

    gl.glBegin(GL2.GL_POLYGON)
    gl.glColor3f(1.0f, 0.0f, 0.0f) // 赤
    gl.glVertex2f(-0.9f, -0.9f)
    gl.glColor3f(0.0f, 1.0f, 0.0f) // 緑
    gl.glVertex2f(0.9f, -0.9f)
    gl.glColor3f(0.0f, 0.0f, 1.0f) // 青
    gl.glVertex2f(0.9f, 0.9f)
    gl.glColor3f(1.0f, 1.0f, 0.0f) // 黄
    gl.glVertex2f(-0.9f, 0.9f)

    angle += 0.5f
  }

  override def dispose(drawable: GLAutoDrawable): Unit = {

  }

  override def reshape(drawable: GLAutoDrawable, x: Int, y: Int, width: Int, height: Int): Unit = {

  }
}