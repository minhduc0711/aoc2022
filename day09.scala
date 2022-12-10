import scala.io.Source
import scala.annotation.tailrec
import scala.math.round
import scala.math.abs
import scala.collection.mutable

def parseMotion(s: String) =
  val Array(direction, numRepeats) = s.split(" ")
  val motion = direction match
    case "L" => (-1, 0)
    case "R" => (1, 0)
    case "U" => (0, 1)
    case "D" => (0, -1)
  (Point(motion(0), motion(1)), numRepeats.toInt)

case class Point(x: Int, y: Int):
  def +(p: Point): Point = Point(x + p.x, y + p.y)

  def -(p: Point): Point = Point(x - p.x, y - p.y)

  def fromTuple(tup: (Int, Int)) = Point(tup(0), tup(1))

  // Used to compute the appropriate tail motion from head-tail displacement
  // if a coordinate is 1, it stays the same
  // if a coordinate is 2, it is divided by 2
  def specialDiv2: Point =
    def div2(n: Int): Int = if abs(n) == 1 then n else n / 2
    Point(div2(x), div2(y))

  def toTuple = (x, y)

  override def toString(): String = s"($x, $y)"

def sqr(x: Int) = x * x

def sqrNorm(p: Point) = sqr(p.x) + sqr(p.y)

def simulateRope(numKnots: Int, lines: Seq[String]) =
  val initialKnotsPos = (for i <- 0 to numKnots - 1 yield i -> Point(0,0)).toMap
  val initial = (initialKnotsPos, Set(Point(0,0)))
  val (knotsPos, visitedPos) = lines.foldLeft(initial) { case ((knotsPos, visitedPos), line) =>
    val (motion, numRepeats) = parseMotion(line)
    @tailrec
    def loop(numRepeats: Int, knotsPos: Map[Int, Point], visitedPos: Set[Point]):
        (Map[Int, Point], Set[Point]) =
      if numRepeats == 0 then
        (knotsPos, visitedPos)
      else
        var updatedKnotsPos = knotsPos
        for List(i, j) <- (0 to numKnots - 1).toList.sliding(2, 1) do
          val newHeadPos = if i == 0 then
            val newFirstHeadPos = updatedKnotsPos(i) + motion
            updatedKnotsPos = updatedKnotsPos.updated(0, newFirstHeadPos)
            newFirstHeadPos
          else
            updatedKnotsPos(i)
          val tailPos = updatedKnotsPos(j)
          val displacement = newHeadPos - tailPos
          val sqrDist = sqrNorm(displacement)
          // Determines if tail needs to move or not
          val tailMotion = if sqrDist >= 4 then
            displacement.specialDiv2
          else
            Point(0, 0)
          updatedKnotsPos = updatedKnotsPos.updated(j, tailPos + tailMotion)
        loop(numRepeats - 1, updatedKnotsPos, visitedPos + knotsPos(knotsPos.size - 1))
    loop(numRepeats, knotsPos, visitedPos)
  }
  visitedPos.size

@main def main9() =
  val filename = "input/day09.txt"
  val lines = Source.fromFile(filename).getLines().toSeq

  println(s"part 1: ${simulateRope(2, lines)}")
  println(s"part 2: ${simulateRope(10, lines)}")
