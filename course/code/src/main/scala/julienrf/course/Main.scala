package julienrf.course

import doodle.core
import doodle.core._
import doodle.jvm._

object Main extends App {

  // game of life
  gameoflife.GameOfLife.start(100)


  val xs= 1 to 10
  xs.foldLeft(0)((acc, x) => acc + x)

  // --- Program entry point

  // draw(Circle(50)) // Complete the exercises and change this line to test your code (e.g. `draw(circles(10))`)

//  draw(circles(1))
//  draw(circles(2))
//  draw(circles(3))

  def weight(width: Int) = Rectangle(width, 100) fillColor Color.black

  def barbell(weight: Image) = {
    val bar = Rectangle(200, 20) fillColor Color.gray
    weight beside bar beside weight
  }

  // draw(barbell(weight(10)) above barbell(weight(20)))

  val unit = barbell(weight(15))
  def barbells(n: Int): Image = if (n==1) unit else unit above barbells(n - 1)

  // draw(barbells(5))


  // draw(Rectangle(10,10).at(15,30) on Rectangle(20, 20))

  // draw(spiral(1))
  // draw(spiral(2))
  // draw(spiral(3))
  // draw(spiral(8))
  // draw(spiral(10))

  // draw(sierpinski(1))
  // draw(sierpinski(2))
  // draw(sierpinski(3))
  // draw(sierpinski(5))

  // --- Exercises

  def circles(n: Int): Image = {
    val circle = Circle(10 * n + 15)
    if (n == 1) circle else circle on circles(n - 1)
  }

  def spiral(n: Int): Image = {
    val angle = Angle.degrees(n * 36 % 360)
    val dist = n * 20
    val x = angle.cos * dist
    val y = angle.sin * dist
    val circle = Circle(n * 5).at(x, -y)
    if (n == 1) circle else circle on spiral(n - 1)
  }

  def sierpinski(n: Int): Image = {
    if (n==1) Triangle(15, 15 * Math.sqrt(3) / 2) fillColor Color.black
    else {
      val motif = sierpinski(n - 1)
      motif above (motif beside motif)
    }
  }

  // draw(sierpinski(5))

  sealed trait FitnessDevice
  case class Barbell(load: Int, lenght: Int) extends FitnessDevice
  case class Mat(width: Int, heigth: Int) extends FitnessDevice

  def fitnessDeviceImage(fitnessDevice: FitnessDevice): Image = fitnessDevice match {
    case barbell: Barbell => ???
    case Mat(width, heigth) => Rectangle(width, heigth) fillColor Color.blue
  }

  // val gymImage = fitnessDeviceImage(Barbell(10,100)) on fitnessDeviceImage(Mat(200, 300))
  // draw(gymImage)

  sealed trait Niveau
  object Niveau {
    case object Facile extends Niveau
    case object Moyen extends Niveau
    case object Difficile extends Niveau
  }
  case class Cours(nom: String, niveau: Niveau)

  val scala = Cours("Scala", Niveau.Facile)

}

