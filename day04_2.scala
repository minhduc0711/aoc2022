import scala.io.Source

def parsePair(s: String) =
  val pair = s.split(",")
  val rangePair = pair.map(rangeStr => rangeStr.split("-"))
    .map(rangeArr => rangeArr.map(_.toInt))
    .map(rangeArr => Range(rangeArr(0), rangeArr(1)).inclusive)
  rangePair

@main def day04_2() =
  val filename = "input/day04.txt"

  val text = Source.fromFile(filename).mkString
  val pairs = text.split("\n").map(parsePair)

  val res = pairs
    .filter(pair => !pair(0).intersect(pair(1)).isEmpty)
    .length

  println(res)
