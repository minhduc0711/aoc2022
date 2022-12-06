import scala.io.Source
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

def find(text: String, step: Int) =
  val iter = text.sliding(step, 1)

  var cnt = step - 1
  breakable {
    while true do
      val charSet = iter.next.toSet
      cnt += 1
      if charSet.size == step then
        break
  }
  cnt

@main def main6() =
  val filename = "input/day6.txt"
  val text = Source.fromFile(filename).mkString

  println("Part 1: " + find(text, 4))
  println("Part 2: " + find(text, 14))
