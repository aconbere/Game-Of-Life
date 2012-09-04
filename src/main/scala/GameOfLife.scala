class GameOfLife(grid:Grid,
                 history:List[Grid]) {

  def this(grid:Grid) = this(grid, List[Grid]())

  override def toString() = grid.toString()

  def birthCell(x:Int, y:Int) = grid.setCell(x,y,true)
  def killCell(x:Int, y:Int) = grid.setCell(x,y,false)

  def next():GameOfLife = {
    new GameOfLife(grid.next(), grid :: history)
  }
}
