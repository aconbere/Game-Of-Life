package com.conbere.life

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

class GridShardActor(shard:Rect) extends Actor {
  def act() = {
    loop {
      react {
        case Next(grid:Grid) => reply(grid.next(shard))
      }
    }
  }
}

class Runner(grid:Grid, renderer:Renderer) extends Actor {
  var state:State = RUNNING
  var history:List[Grid] = List()

  val workers = grid.shards.map(s => new GridShardActor(s))
  val _stitch = M.stitch(grid.height, grid.width) _

  def next() = {
    val n = _stitch(workers.map(w => w !! Next(grid))
                           .map(f =>
                              f() match {
                                case newGrid:Grid => newGrid
                                case _ => throw new Exception("oops")
                              }))
    history = history :+ n
    n
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
    println("wtf")
  )

  def act() = {
    def run(g:Grid):Unit = {
      state match {
        case RUNNING =>
          renderer.render(g)
          run(next())
        case REVERSED =>
          renderer.render(g)
          history match {
            case x::xs => run(x)
            case Nil => state = PAUSED
          }
        case STOPPED =>
          renderer.dispose()
          System.exit(0)
        case PAUSED =>
          run(g)
        case _ =>
      }
    }
    run(grid)
  }

}
