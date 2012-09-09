import java.awt.event.KeyEvent.{ VK_ESCAPE
                               , VK_SPACE
                               , VK_LEFT
                               , VK_RIGHT
                               }

sealed trait State { def name: String }

case object RUNNING extends State { val name = "Running" }
case object PAUSED extends State { val name = "Paused" }
case object STOPPED extends State { val name = "Stopped" }
case object REVERSED extends State { val name = "Reversed" }

class Runner(game:GameOfLife, renderer:Renderer) {
  var state:State = RUNNING

  renderer.addKeyListener((keycode:Int) =>
    keycode match {
      case VK_ESCAPE => state = STOPPED
      case VK_SPACE =>
        state match {
          case PAUSED => state = RUNNING
          case RUNNING => state = PAUSED
          case _ =>
        }
      case VK_LEFT => state = REVERSED
      case VK_RIGHT => state = RUNNING
      case _ =>
    }
  )

  renderer.addMouseListener((button:Int, x:Int, y:Int) =>
    println("wtf")
    //println((button, x, y))
  )

  def run(g:GameOfLife=game):Unit = {
    state match {
      case RUNNING =>
        renderer.render(g)
        run(g.next)
      case REVERSED =>
        renderer.render(g)
        run(g.previous)
      case STOPPED =>
        renderer.dispose()
        System.exit(0)
      case PAUSED =>
        renderer.render(g)
        run(g)
      case _ =>
    }
  }
}
