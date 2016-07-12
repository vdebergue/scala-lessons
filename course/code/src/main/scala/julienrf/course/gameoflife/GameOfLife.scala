package julienrf.course.gameoflife

import doodle.core._

object GameOfLife {

  start(10)

  def start(size: Int) {
    val clock: Events[Unit] = Events.every(200)
    val worldInit: World = ???
    val worlds: Events[World] = clock.fold(worldInit) { (_ , world) =>
      world.next
    }

    val images: Events[Image] = worlds.map(world => image(world))

    Ui.show(images)
  }

  def image(world: World): Image = ???

}

case class World() {
  def next: World = ???
}

object World {

}
