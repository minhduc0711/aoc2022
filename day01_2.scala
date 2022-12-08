import scala.io.Source

@main def day01_2() =
  val filename = "input/day01.txt"

  val text = Source.fromFile(filename).mkString
  val inventories: Array[String] = text.split("\n\n")

  val a = inventories.map(_.split("\n"))
                      .map(_.map(_.toString.toInt))
                      .map(_.reduce(_ + _))
                      .sortWith(_ > _)
                      .take(3)
                      .reduce(_ + _)
  println(a)
  // println(a.mkString(" "))
