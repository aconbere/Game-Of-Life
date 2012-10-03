package com.conbere.life

import com.codahale.logula.Logging

import java.awt.event.KeyEvent.{ VK_ESCAPE
                               , VK_SPACE
                               , VK_LEFT
                               , VK_RIGHT
                               }

import scala.actors._

sealed trait State { def name: String }

case object RUNNING  extends State { val name = "Running" }
case object PAUSED   extends State { val name = "Paused" }
case object STOPPED  extends State { val name = "Stopped" }
case object REVERSED extends State { val name = "Reversed" }

case class Next(val grid:Grid)

class GridShardActor(shard:Rect) extends Actor with Logging {
  def act() = {
    loop {
      react {
        case Next(grid:Grid) =>
          reply(grid.next(shard))
        case _ => exit()
      }
    }
  }
}

class Runner(grid:Grid, renderer:Renderer) extends Logging {
  var state:State = RUNNING
  var updates:List[(Int, Int)] = List()

  val workers = grid.shards.map(s => new GridShardActor(s))
  workers.foreach(w => w.start())

  val _stitch = M.stitch(grid.height, grid.width) _

  def next(g:Grid, h:List[Grid]) = {
    val n = _stitch(workers.map(w => w !! Next(g))
                           .map(f =>
                              f() match {
                                case newGrid:Grid => newGrid
                                case _ => throw new Exception("oops")
                              }))
    (n, n +: h)
  }

  renderer.addKeyListener((keycode:Int) =>
    keycode match {
      case VK_ESCAPE => state = STOPPED
      case VK_LEFT => state = REVERSED
      case VK_RIGHT => state = RUNNING
      case VK_SPACE =>
        state match {
          case PAUSED => state = RUNNING
          case RUNNING => state = PAUSED
          case _ =>
        }
      case _ =>
    }
  )

  renderer.addMouseListener((button:Int, x:Int, y:Int) =>
    button match {
      case 1 =>
        state match {
          case PAUSED =>
            log.info("new updates")
            updates = (x / 4, y / 4) +: updates
          case _ =>
        }
      case _ =>
        log.info("button:%s".format(button))
    }
  )

  def stop() = {
    renderer.dispose()
    workers.foreach(w => w ! None)
    System.exit(0)
  }


  renderer.addWindowListener(() => stop())

  def run() = {
    def inner(g:Grid, h:List[Grid]):Unit = {
      state match {
        case RUNNING =>
          log.info("RUNNING")
          renderer.render(g)
          (inner _).tupled(next(g, h))
        case REVERSED =>
          log.info("REVERSED")
          renderer.render(g)
          h match {
            case x::xs => inner(x, xs)
            case Nil =>
              state = PAUSED
              inner(g,h)
          }
        case STOPPED =>
          log.info("STOPPED")
          stop()
        case PAUSED =>
          log.info("PAUSED:%s".format(updates.length))
          for ((x:Int, y:Int) <- updates) {
            log.info("flipping")
            g.flipCell(x,y)
          }
          updates = List()
          renderer.render(g)
          inner(g, h)
        case _ =>
      }
    }
    inner(grid, List())
  }
}
