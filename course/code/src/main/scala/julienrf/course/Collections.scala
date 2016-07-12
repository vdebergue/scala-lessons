package julienrf.course

import doodle.core._

object Collections {

  /**
   * @return a sequence whose elements are the elements of `xs` incremented by one
   */
  def inc(xs: Seq[Int]): Seq[Int] = ???

  /**
   * @return a sequence whose elements are the length of the elements of `ss`
   */
  def lengths(ss: Seq[String]): Seq[Int] = ???

  def sum(xs: Seq[Int]): Int = ???

  def product(xs: Seq[Int]): Int = ???

  /**
   * Define a method that takes a sequence of Mat and returns their areas if this one is greater than 1000
   */
  // def largeEnough(mats: Seq[Mat]): Seq[Int] = ???

  /**
   * Returns `n` concentric circles  (recursive implementation)
   */
  def circlesRec(n: Int): Seq[Circle] = {
    if (n == 0) Seq.empty else Circle(10 * n) +: circles(n - 1)
    // if (n == 1) Seq(Circle(10 * n)) else Circle(10 * n) +: circles(n - 1)
  }


  /**
   * Returns `n` concentric circles
   */
  def circles(n: Int): Seq[Circle] = (1 to n).map(i => Circle(10 * i))


  def spiral(n: Int): Seq[Image] = (0 until n).map { i =>
    val dist = 10 * i
    val angle = Angle.degrees(360 * i)
    val x = dist * angle.cos
    val y = dist * angle.sin
    Circle(5 * i).at(x, y)
  }

  def spiralRec(n: Int): Seq[Image] = {
    val dist = 10 * n
    val angle = Angle.degrees(360 * n)
    val x = dist * angle.cos
    val y = dist * angle.sin
    val circle = Circle(5 * n).at(x, y)
    if (n == 1) Seq(circle) else circle +: spiralRec(n - 1)
  }

  // use foldLeft
  val emptyImage: Image = Rectangle(0,0)
  // def stack(images: Seq[Image]): Image = images.foldLeft(emptyImage)((acc, image) => acc on image)
  def stack(images: Seq[Image]): Image = images.foldLeft(emptyImage)(_ on _)

  val xs: Seq[Int] = 1 to 10
  val total: Int = xs.foldLeft(0)((acc, x) => acc + x)

  val total2 = xs.foldLeft(0)(_ + _)

  def stackRec(images: Seq[Image]): Image = images match {
    case image +: Nil => image
    case image +: autresImages => image on stackRec(autresImages)
  }

  // draw(stackRec(spiralRec(10)))

  case class Mat(width: Int, length: Int) {
    def smaller: Option[Mat] = {
      if (width < 50 || length < 100) None
      else Some(Mat(width - 40, length - 80))
    }
  }

  val mat = Mat(100,200)
  val smallMatOption: Option[Mat] = mat.smaller

  // Write a method that takes a Mat as parameter,
  //    tries to get a smaller mat
  //    and returns its area
  //    only if it is higher than 1000
  def smallerButLargeEnough(mat: Mat): Option[Int] = mat.smaller.map(m => m.width * m.length).filter(_ > 1000)

  // takes a sequence of Mat and returns their areas if this one is greater than 1000
  def largeEnough(mats: Seq[Mat]): Seq[Int] = mats match {
    case Nil => Nil
    case mat +: tail =>
      val area = mat.width * mat.length
      if (area > 1000) area +: largeEnough(tail) else largeEnough(tail)
  }

  val mats = Seq(Mat(2,2), Mat(200, 200), Mat(10, 30), Mat(300, 10))
  largeEnough(mats) == Seq(40000, 3000)

  val areas = mats.map(mat => mat.width * mat.length)
  val largerThanOneHundred = mats.filter(mat => mat.width > 100)

  def largeEnoughMapFilter(mats: Seq[Mat]): Seq[Int] = mats.map(mat => mat.width * mat.length).filter(_ > 1000)

}





