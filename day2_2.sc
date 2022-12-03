import scala.io.Source

def encodeChoice(s: String): Int =
  s match
    case "A" => 0
    case "B" => 1
    case "C" => 2

def getMatchResult(oppChoice: String, outcome: String): Int =
  val oppChoiceId: Int = encodeChoice(oppChoice)
  outcome match
    // we lose
    case "X" => (oppChoiceId + 2) % 3 + 1
    // we draw
    case "Y" => oppChoiceId + 1 + 3
    // we win
    case "Z" => (oppChoiceId + 1) % 3 + 1 + 6

@main def day2_2() =
  val filename = "input/day2.txt"

  val text = Source.fromFile(filename).mkString
  val matches = text.split("\n")
    .map(_.split(" "))
  val totalPt: Int = matches.map(arr => getMatchResult(arr(0), arr(1)))
    .reduce(_ + _)

  println(totalPt)
