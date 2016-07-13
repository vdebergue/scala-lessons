package julienrf.course.gameoflife

import doodle.core._

object GameOfLife {

  def start(size: Int) {
    val clock: Events[Unit] = Events.every(200)
    val worldInit: World = World.init(size)
    // val worldInit: World = World.test
    // val worldInit = World.blinker
    val worlds: Events[World] = clock.fold(worldInit) { (_ , world) =>
      world.next2
    }

    val images: Events[Image] = worlds.map(world => image2(world))

    Ui.show(images)
  }

  val emptyImage: Image = Rectangle(0,0)

  val cellSize = 10
  def cellImage(cell: Cell): Image = {
    val color = if (cell == Alive) Color.green else Color.white
    Rectangle(cellSize, cellSize).fillColor(color)
  }

  def image(world: World): Image = {
    val images: Seq[Image] = for {
      i <- 0 until world.cells.size
      j <- 0 until world.cells(i).size
    } yield {
      val cell = world.cells(i)(j)
      val x = cellSize * j
      val y = cellSize * i
      cellImage(cell).at(x,y)
    }
    images.foldLeft(emptyImage)(_ on _)
  }

  def image2(world: World): Image = {
    val rowImages = world.cells.map{ row =>
      val cellImages = row.map(cellImage)
      cellImages.foldLeft(emptyImage)(_ beside _)
    }
    rowImages.foldLeft(emptyImage)(_ above _)
  }

}

sealed trait Cell
case object Alive extends Cell
case object Dead extends Cell

case class World(cells: Seq[Seq[Cell]]) {

  def next: World = {
    val newCells = (0 until cells.length).map { y =>
      val row = cells(y)
      (0 until row.length).map { x =>
        val cell = row(x)
        decideNextCell(cell, x, y)
      }
    }
    World(newCells)
  }

  def next2: World = {
    val newCells = Seq.tabulate(cells.length, cells.length) { (j,i) =>
      val cell = getCell(i, j)
      decideNextCell(cell, i, j)
    }
    // println("------------------------")
    World(newCells)
  }

  def decideNextCell(cell: Cell, x: Int, y: Int): Cell = {
    val neighbours = getNeighbours(x,y)
    val aliveNeighbours = neighbours.count(_ == Alive)

    val res = {
      if (aliveNeighbours == 0 || aliveNeighbours == 1) Dead
      else if ((aliveNeighbours == 2 || aliveNeighbours == 3) && cell == Alive) cell
      else if (aliveNeighbours == 3 && cell == Dead) Alive
      else Dead
    }
    // println(s"Deciding for x = $x y = $y - Alive neighbours = $aliveNeighbours: $cell => $res ")
    res
  }

  def getNeighbours(x: Int, y: Int): Seq[Cell] = {
    for {
      i <- (x - 1) to (x + 1)
      j <- (y - 1) to (y + 1)
      if (i != x || j != y)
    } yield getCell(i,j)
  }

  def getCell(x: Int, y: Int): Cell = {
    def posMod(x: Int, n: Int): Int = ((x % n) + n) % n
    val xMod = posMod(x, cells.size)
    val yMod = posMod(y, cells(xMod).size)
    cells(yMod)(xMod)
  }
}

object World {
  def init(size: Int): World = {
    val cells = Vector.fill(size, size){
      if(scala.util.Random.nextInt(10) > 5) Alive else Dead
    }
    World(cells)
  }

  def test: World = {
    World(
      Seq(
        Seq(Alive, Alive, Dead),
        Seq(Dead, Dead, Dead),
        Seq(Dead, Dead, Dead)
      )
    )
  }

  def blinker: World = {
    World(
      Seq(
        Seq(Dead, Dead, Dead, Dead, Dead),
        Seq(Dead, Dead, Alive, Dead, Dead),
        Seq(Dead, Dead, Alive, Dead, Dead),
        Seq(Dead, Dead, Alive, Dead, Dead),
        Seq(Dead, Dead, Dead, Dead, Dead)
      )
    )
  }
}
