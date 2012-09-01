import collection.immutable.HashMap
import collection.immutable.List

class Cell() {}

class Grid(val width:Int,
           val height:Int,
           val cells:HashMap[(Int, Int), Cell]) {

  def getCell(x:Int, y:Int) = {
    cells.get((x, y))
  }

  def setCell(x:Int, y:Int, c:Cell) = {
    new Grid(width, height, cells + ((x, y) -> c))
  }

  def deleteCell(x:Int, y:Int) = {
    new Grid(width, height, cells - ((x, y)))
  }

  def getNeighbors(x:Int, y:Int) = {
    List( getCell(x - 1, y)
        , getCell(x + 1, y)
        , getCell(x, y + 1)
        , getCell(x, y - 1)
        )
  }
}
