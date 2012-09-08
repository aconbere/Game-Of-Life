import java.awt.Canvas
import java.awt.Color
import java.awt.Frame
import java.awt.Graphics2D
import java.awt.GraphicsConfiguration
import java.awt.GraphicsEnvironment
import java.awt.Toolkit
import java.awt.Transparency
import java.awt.Rectangle

import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.event.KeyListener
import java.awt.event.KeyEvent

import java.awt.image.BufferStrategy
import java.awt.image.BufferedImage

import javax.swing.JFrame
import javax.swing.WindowConstants

class LifeKeyListener(callback:(Int) => Unit) extends KeyListener {
  def keyPressed(event:KeyEvent) = {
    callback(event.getKeyCode())
  }

  def keyReleased(event:KeyEvent) = {}
  def keyTyped(event:KeyEvent) = {}
}

class Renderer(width:Int, height:Int) {
  val config = GraphicsEnvironment
                        .getLocalGraphicsEnvironment()
                        .getDefaultScreenDevice()
                        .getDefaultConfiguration()

  val canvas = new Canvas(config)
  canvas.setSize(width, height)

  val frame = new JFrame()
  frame.addWindowListener(new WindowAdapter() {
    override def windowClosing(e : WindowEvent) = {
      System.exit(0)
    }
  })
  frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
  frame.setSize(width, height);
  frame.add(canvas, 0);
  frame.setVisible(true);

  // can't create buffer strategies until visible
  canvas.createBufferStrategy(2)
  val strategy = canvas.getBufferStrategy()

  val background = config.createCompatibleImage(width, height, Transparency.OPAQUE)

  def addKeyListener(callback:(Int) => Unit) {
    frame.addKeyListener(new LifeKeyListener(callback))
  }

  def dispose() = {
    frame.dispose()
  }

  def render(game:GameOfLife):Unit = {
    val bgGraphics = background.createGraphics()
    bgGraphics.setColor(Color.WHITE)
    bgGraphics.fill(new Rectangle(0,0,height,width))

    bgGraphics.setColor(Color.BLACK)

    game.foreach((i,j,live) => if(live) bgGraphics.fill(new Rectangle(i*4,j*4,4,4)))

    val graphics = strategy.getDrawGraphics()
    graphics.drawImage(background, 0, 0, null);

    strategy.show()
    Toolkit.getDefaultToolkit().sync()
    Thread.sleep(1000)

    graphics.dispose()
  }
}


