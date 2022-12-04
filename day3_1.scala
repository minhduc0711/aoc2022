import scala.io.Source

def getCompartments(s: String) =
  val len = s.length
  List(
    s.substring(0, len / 2).split("").toSet,
    s.substring(len / 2, len).split("").toSet
  )

def getPriority(itemType: Char) =
  val offset = if itemType.isUpper then 64 - 26 else 96
  itemType.toInt - offset


@main def day3_1() =
  val filename = "input/day3.txt"

  val text = Source.fromFile(filename).mkString
  val rucksacks: Array[String] = text.split("\n")

  val res = rucksacks.map(getCompartments(_))
    .map(l => l(0).intersect(l(1)).head)
    .map(_.toCharArray()(0))
    .map(getPriority)
    .reduce(_ + _)

  println(res)
