class Grid(val width:Int,
           val height:Int,
           val cells:Array[Array[Int]]) {

  def this(width:Int, height:Int) = {
    this(width, height, Array.ofDim[Int](width, height))
  }

  def setCell(x:Int, y:Int, s:Boolean) = {
    cells(x)(y) = if (s) 1 else 0
  }

  override def toString() = {
    cells.map(row =>
        row.map(i => if(i==1) "x"  else ".").mkString("")
        ) .mkString("\n")
  }

  def wrap(constraint:Int, x:Int):Int = {
    if (x >= constraint) {
      x - constraint
    } else if ( x < 0 ) {
      constraint + x
    } else {
      x
    }
  }

  def copy() = {
    val dest = Array.ofDim[Int](width, height)
    Array.copy(cells, 0, dest, 0, cells.length)
    new Grid(width, height, dest)
  }

  def getNeighbors(x:Int, y:Int) = {
    val hc = wrap(height, _:Int)
    val wc = wrap(width, _:Int)

    List(
          cells(wc(x - 1))(hc(y + 1))
        , cells(wc(x - 1))(hc(y))
        , cells(wc(x - 1))(hc(y - 1))

        , cells(wc(x))(hc(y + 1))
        , cells(wc(x))(hc(y - 1))

        , cells(wc(x + 1))(hc(y + 1))
        , cells(wc(x + 1))(hc(y))
        , cells(wc(x + 1))(hc(y - 1))
        )
  }

  def next() = {
    val _next = new Grid(width, height, Array.ofDim[Int](width, height))

    for ((col, i) <- cells.zipWithIndex) {
      for ((row, j) <- col.zipWithIndex) {
        val neighbors = getNeighbors(i,j)
        val count = neighbors.sum
        if (count > 1 && count < 4) {
          _next.setCell(i,j,true)
        }
      }

    }
    _next
  }
}
