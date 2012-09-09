object Life {
  val grid = new Grid(100, 100)
  val life = new GameOfLife(grid)

  life.birthCell(5,6)
  life.birthCell(5,7)
  life.birthCell(6,7)
  life.birthCell(7,7)
  life.birthCell(5,8)

  def main(args:Array[String]) = {
    new Runner(life, new Renderer(grid.width * 4, grid.height * 4)).run()
  }
}
