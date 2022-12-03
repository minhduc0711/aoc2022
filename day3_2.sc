import scala.io.Source

def getPriority(itemType: Char) =
  val offset = if itemType.isUpper then 64 - 26 else 96
  itemType.toInt - offset


@main def day3_2() =
  val filename = "input/day3.txt"

  val text = Source.fromFile(filename).mkString
  val rucksacks: Array[String] = text.split("\n")

  val res = rucksacks
    .map(_.split("").toSet)
    .sliding(3, 3).toList
    .map(arr => arr(0).intersect(arr(1)).intersect(arr(2)).head)
    .map(_.toCharArray()(0))
    .map(getPriority)
    .reduce(_ + _)

  println(res)
