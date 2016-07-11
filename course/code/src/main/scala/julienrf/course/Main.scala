package julienrf.course

import doodle.core
import doodle.core._
import doodle.jvm._

object Main extends App {

  // --- Program entry point

  // draw(Circle(50)) // Complete the exercises and change this line to test your code (e.g. `draw(circles(10))`)

//  draw(circles(1))
//  draw(circles(2))
//  draw(circles(3))


  // draw(Rectangle(10,10).at(15,30) on Rectangle(20, 20))

  // draw(spiral(1))
  // draw(spiral(2))
  // draw(spiral(3))
  // draw(spiral(8))
  // draw(spiral(10))


  draw(sierpinski(1))
  draw(sierpinski(2))
  draw(sierpinski(3))
  draw(sierpinski(5))

  // --- Exercises

  def circles(n: Int): Image = {
    val circle = Circle(10 * n + 15)
    if (n == 1) circle else circle on circles(n - 1)
  }

  def spiral(n: Int): Image = {
    val angle = Angle.degrees(n * 36 % 360)
    val dist = n * 20
    val circle = Circle(5 * n).at(dist * angle.cos, - dist * angle.sin)
    if (n == 1) circle else circle on spiral(n - 1)
  }

  def sierpinski(n: Int): Image = {
    if (n == 1) Triangle(15, 15 * Math.sqrt(3) / 2) fillColor Color.black
    else {
      val last = sierpinski(n - 1)
      last above (last beside last)
    }
  }

}