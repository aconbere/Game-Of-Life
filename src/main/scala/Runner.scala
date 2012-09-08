import java.awt.event.KeyEvent.VK_ESCAPE

class Runner(game:GameOfLife, renderer:Renderer) {
  var running = true

  renderer.addKeyListener((keycode:Int) =>
    keycode match {
      case VK_ESCAPE =>
        running = false
      case _ =>
    }
  )

  def run(g:GameOfLife=game):Unit = {
    if (running) {
      renderer.render(g)
      run(g.next)
    } else {
      renderer.dispose()
      System.exit(0)
    }
  }
}
