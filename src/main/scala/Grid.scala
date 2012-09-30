package com.conbere.life

package object M {
  type Matrix = Array[Array[Boolean]]

  /* Creates a 2D array
   *
   * width ->
   * [x o o o o
   *  x o o o o
   *  o o o o o
   *  o o o o o
   *  x o o o o]
   *
   * m = lifeMatrix(5, 5)
   * m(0) => [x, x, o, o, x] // retreives a column
   * m(0)(0) => x // retreives a row element
   *
   */
  def lifeMatrix(width:Int, height:Int) = {
    Array.ofDim[Boolean](width, height)
  }

  def stitch(width:Int, height:Int)(grids:List[Grid]) = {
    val m = lifeMatrix(width, height)

    // copies the top of the source grids into the top
    // of the destination matrix
    Array.copy(grids(0).cells, 0, m, 0, m.length)
    Array.copy(grids(1).cells, 0, m, 0, m.length)

    for (j <- width/2 to width) {
      val col = m(j)
      Array.copy(grids(2).cells(j), 0, col, height/2, col.length)
    }

    for (j <- 0 to width/2) {
      val col = m(j)
      Array.copy(grids(3).cells(j), 0, col, height/2, col.length)
    }

    new Grid(width, height, m)
  }
}

class Rect(val x1:Int, val y1:Int,
           val x2:Int, val y2:Int) {
  def width = x1 - x2
  def height = y1 - y2
}

class Grid(val width:Int,
           val height:Int,
           val cells:M.Matrix
           ) {

  val d = new Rect(0,0,width,height)

  def this(width:Int, height:Int) = {
    this(width, height, M.lifeMatrix(width, height))
  }

  def setCell(x:Int, y:Int, s:Boolean) = {
    cells(x)(y) = s
  }

  def getCells():M.Matrix = {
    val dest = M.lifeMatrix(width, height)
    Array.copy(cells, 0, dest, 0, cells.length)
    return dest
  }

  override def toString() = {
    cells.map(row =>
      row.map(i => if (i) "x"  else ".").mkString("")
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

    val left  = wc(x - 1)
    val right = wc(x + 1)
    val up    = hc(y + 1)
    val down  = hc(y - 1)

    List( cells(left)(up)
        , cells(left)(y)
        , cells(left)(down)

        , cells(x)(up)
        , cells(x)(down)

        , cells(right)(up)
        , cells(right)(y)
        , cells(right)(down)
        )
  }

  def shards = {
    /* This is my annoying comment to help
     * me remember how coordinates work
     * because I'm an idiot
     *
     * (x1,y1)   (x2/2, y1)   (x2,y1)
     *
     * (x1,y2/2) (x2/2, y2/2) (x2,y2/2)
     *
     * (x1,y2)   (x2/2, y2)   (x2,y2)
     *
     * Each shard is listed in clockwise order
     */
    List( new Rect(d.x1,   d.y1,
                   d.x2/2, d.y2/2),
          new Rect(d.x2/2, d.y2/2,
                   d.x2,   d.y2),
          new Rect(d.x1,   d.y2/2,
                   d.x2/2, d.y2),
          new Rect(d.x2/2, d.y1,
                   d.x2,   d.y2/2)
        )
  }

  def foreach(cb:(Int, Int, Boolean) => Unit):Unit = {
    for ((row, i) <- cells.zipWithIndex;
         (cell, j) <- row.zipWithIndex) {
      cb(i, j, cell)
    }
  }


  def next(rect:Rect=d):Grid= {
    val _next = M.lifeMatrix(rect.width, rect.height)

    for (i <- rect.x1 to rect.x2;
         j <- rect.y1 to rect.y2) {
      val count = getNeighbors(i,j).map(x => if (x) 1 else 0 ).sum
      val cell = cells(i)(j)

      if (count == 3 || (cell && count == 2)) {
        _next(i)(j) = true
      }
    }

    return new Grid(width, height, _next)
  }
}
