package com.conbere.life

object Life {
  val grid = new Grid(100, 100)

  grid.setCell(5, 6, true)
  grid.setCell(5, 7, true)
  grid.setCell(6, 7, true)
  grid.setCell(7, 7, true)
  grid.setCell(5, 8, true)

  def main(args:Array[String]) = {
    new Runner(grid, new Renderer(grid.width * 4, grid.height * 4)).act()
  }
}
