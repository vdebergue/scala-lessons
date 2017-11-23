package julienrf.course.gameoflife

object GameOfLife {

  def start(size: Int) {
    val clock: Events[Unit] = Events.every(200)
    val worldInit: World = World.init(size)
    // val worldInit: World = World.test


    val worlds: Events[World] = ???

    val images: Events[Image] = worlds.map(???)

    Ui.show(images)
  }

}
