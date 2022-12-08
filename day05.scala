import scala.io.Source
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

def parseInput(filename: String) =
  val text = Source.fromFile(filename).mkString

  val stringArr = text.split("\n\n")
  val stackStr = stringArr(0)
  val stackLevels = stackStr.split("\n").reverse
  val numStacks = (stackLevels(0).length + 1) / 4
  val stacks = List.fill(numStacks)(new ListBuffer[Char]())
  val stackIdxs = (0 to numStacks - 1)
  val stackPositions = stackIdxs.map(_*4 + 1)

  stackLevels.slice(1, stackLevels.length).foreach(
    str =>
      for ((idx, pos) <- stackIdxs zip stackPositions
      if (str(pos) != ' ')) stacks(idx).addOne(str(pos))
  )

  val arrangeStr = stringArr(1)
  val pattern: Regex = """move (\d+) from (\d+) to (\d+)""".r
  val arrangements = pattern.findAllMatchIn(arrangeStr)
    .map(m => (m.group(1).toInt, m.group(2).toInt - 1, m.group(3).toInt - 1))
    .toList

  (stacks, arrangements)


@main def main() =
  val filename = "input/day05.txt"
  val (originalStacks, arrangements) = parseInput(filename)

  var stacks = originalStacks.map(_.clone)
  for (cnt, src, dst) <- arrangements do
    for _ <- 1 to cnt do
      val s = stacks(src)
      val d = stacks(dst)
      d.addOne(s.remove(s.length - 1))
  val part1 = stacks.map(_.last).mkString
  println("Part 1: " + part1)

  stacks = originalStacks.map(_.clone)
  for (cnt, src, dst) <- arrangements do
    val s = stacks(src)
    val d = stacks(dst)
    d.addAll(s.takeRight(cnt))
    s.remove(s.length - cnt, cnt)
  val part2 = stacks.map(_.last).mkString
  println("Part 2: " + part2)
