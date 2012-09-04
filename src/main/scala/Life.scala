object Life {
  val life = new GameOfLife(new Grid(20, 20))

  life.birthCell(1,1)
  life.birthCell(2,1)
  life.birthCell(3,1)
  life.birthCell(4,1)
  life.birthCell(2,2)
  life.birthCell(2,3)

  println(life)

  def main(args:Array[String]) = {
    println("Life")
  }
}
