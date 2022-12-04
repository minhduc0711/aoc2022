import scala.io.Source

def encodeChoice(s: String): Int =
  s match
    case "X" => 0
    case "Y" => 1
    case "Z" => 2
    case "A" => 0
    case "B" => 1
    case "C" => 2

def getMatchResult(oppChoice: String, myChoice: String): Int =
  val o: Int = encodeChoice(oppChoice)
  val m: Int = encodeChoice(myChoice)
  if o == (m + 2) % 3 then
    6
  else if o == m then
    3
  else
    0


@main def day2_1() =
  val filename = "input/day2.txt"

  val text = Source.fromFile(filename).mkString
  val matches = text.split("\n")
    .map(_.split(" "))
  val totalPt: Int = matches.map(arr => getMatchResult(arr(0), arr(1)) + encodeChoice(arr(1)) + 1)
    .reduce(_ + _)

  println(totalPt)
