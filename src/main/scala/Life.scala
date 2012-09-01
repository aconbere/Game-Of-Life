import org.lwjgl.opengl.{Display,DisplayMode}

object Life {
  def main(args:Array[String]) = {
    println("Life")
    Display.setDisplayMode(new DisplayMode(100,100))
    Display.create()
  }
}
