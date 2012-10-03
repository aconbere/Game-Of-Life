package com.conbere.life

import com.codahale.logula.Logging

import java.awt.Canvas
import java.awt.Color
import java.awt.Frame
import java.awt.Graphics2D
import java.awt.GraphicsConfiguration
import java.awt.GraphicsEnvironment
import java.awt.Toolkit
import java.awt.Transparency
import java.awt.Rectangle
import java.awt.geom.Line2D

import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import java.awt.event.FocusListener
import java.awt.event.FocusEvent

import java.awt.image.BufferStrategy
import java.awt.image.BufferedImage

import javax.swing.JFrame
import javax.swing.WindowConstants

class LifeFocusListener(callback:() => Unit)
  extends FocusListener with Logging {
  def focusGained(event:FocusEvent) = {
    log.info("LifeFocusListener:focusGained()")
    callback()
  }

  def focusLost(event:FocusEvent) = {
    log.info("LifeFocusListener:focusLost()")
  }
}

class LifeKeyListener(callback:(Int) => Unit)
  extends KeyListener with Logging {
  def keyPressed(event:KeyEvent) = {
    callback(event.getKeyCode())
  }

  def keyReleased(event:KeyEvent) = {}
  def keyTyped(event:KeyEvent) = {}
}

class LifeMouseListener(callback:(Int, Int, Int) => Unit)
  extends MouseListener with Logging {
  def mouseClicked(event:MouseEvent) = {
    callback(event.getButton(), event.getX(), event.getY())
  }
  def mouseEntered(event:MouseEvent) = {}
  def mouseExited(event:MouseEvent) = {}
  def mousePressed(event:MouseEvent) = {}
  def mouseReleased(event:MouseEvent) = {}
}

class Renderer(width:Int, height:Int) extends Logging {
  val config = GraphicsEnvironment
                        .getLocalGraphicsEnvironment()
                        .getDefaultScreenDevice()
                        .getDefaultConfiguration()

  val canvas = new Canvas(config)
  canvas.addFocusListener(new LifeFocusListener(() => log.info("focused canvas")))
  canvas.setSize(width, height)

  val frame = new JFrame()
  frame.addFocusListener(new LifeFocusListener(() => log.info("focused frame")))
  frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
  frame.setSize(width, height);
  frame.add(canvas, 0);
  frame.setVisible(true);

  // can't create buffer strategies until visible
  canvas.createBufferStrategy(2)
  canvas.requestFocusInWindow()
  val strategy = canvas.getBufferStrategy()

  val background = config.createCompatibleImage(width, height, Transparency.OPAQUE)

  def addWindowListener(callback:() => Unit) = {
    frame.addWindowListener(new WindowAdapter() {
      override def windowClosing(e : WindowEvent) = {
        callback()
      }
    })
  }

  def addKeyListener(callback:(Int) => Unit) = {
    canvas.addKeyListener(new LifeKeyListener(callback))
  }

  def addMouseListener(callback:(Int, Int, Int) => Unit) = {
    canvas.addMouseListener(new LifeMouseListener(callback))
  }

  def dispose() = {
    frame.dispose()
  }

  def drawGrid(bgGraphics:Graphics2D) = {
    bgGraphics.setColor(Color.GRAY)
    for (i <- 0 to width) bgGraphics.draw(new Line2D.Double(i * 4, 0, i * 4, height))
    for (j <- 0 to height) bgGraphics.draw(new Line2D.Double(0, j*4, width, j*4))
  }

  def render(grid:Grid):Unit = {
    val bgGraphics = background.createGraphics()
    bgGraphics.setColor(Color.WHITE)
    bgGraphics.fill(new Rectangle(0,0,height,width))

    drawGrid(bgGraphics)

    bgGraphics.setColor(Color.BLACK)

    grid.foreach((i,j,live) => if (live) bgGraphics.fill(new Rectangle(i*4,j*4,4,4)))

    val graphics = strategy.getDrawGraphics()
    graphics.drawImage(background, 0, 0, null);

    strategy.show()
    Toolkit.getDefaultToolkit().sync()
    Thread.sleep(250)

    graphics.dispose()
  }
}
