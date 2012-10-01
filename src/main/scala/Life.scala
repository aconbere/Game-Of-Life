package com.conbere.life

import com.codahale.logula.Logging
import org.apache.log4j.Level

object Life {
  val grid = new Grid(100, 100)

  grid.setCell(5, 6, true)
  grid.setCell(5, 7, true)
  grid.setCell(6, 7, true)
  grid.setCell(7, 7, true)
  grid.setCell(5, 8, true)

  def main(args:Array[String]) = {

    Logging.configure { log =>
      log.registerWithJMX = true

      log.level = Level.ALL

      log.loggers("com.conbere.life") = Level.ALL

      log.console.enabled = true
      log.console.threshold = Level.ALL

      log.file.enabled = true
      log.file.filename = "./logs/life.log"
      log.file.threshold = Level.ALL

      log.syslog.enabled = true
      log.syslog.host = "localhost"
      log.syslog.facility = "LOCAL7"
    }

    new Runner(grid, new Renderer(grid.width * 4, grid.height * 4)).run()
  }
}
