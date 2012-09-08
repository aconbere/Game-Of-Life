class Grid(val width:Int,
           val height:Int,
           val cells:Array[Array[Int]]) {

  def this(width:Int, height:Int) = {
    this(width, height, Array.ofDim[Int](width, height))
  }

  def setCell(x:Int, y:Int, s:Boolean) = {
    cells(x)(y) = if (s) 1 else 0
  }

  def getCells() = {
    val dest = Array.ofDim[Int](width, height)
    Array.copy(cells, 0, dest, 0, cells.length)
    dest
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
    new Grid(width, height, getCells())
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
      for ((cell, j) <- col.zipWithIndex) {
        val count = getNeighbors(i,j).sum
        if (cell == 1) {
          if ( count < 2) {
            _next.setCell(i,j,false)
          } else if ( count > 3) {
            _next.setCell(i,j,false)
          } else {
            _next.setCell(i,j,true)
          }
        } else {
          if ( count == 3 ) {
            _next.setCell(i,j,true)
          }
        }
      }
    }
    _next
  }
}
