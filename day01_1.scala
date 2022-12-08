import scala.io.Source

@main def day01_1() =
  val filename = "input/day01.txt"

  val text = Source.fromFile(filename).mkString
  val inventories: Array[String] = text.split("\n\n")

  val a = inventories.map(_.split("\n"))
                      .map(_.map(_.toString.toInt))
                      .map(_.reduce(_ + _))
                      .reduce(_ max _)
  println(a)
  // println(a.mkString(" "))
