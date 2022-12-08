import scala.io.Source

def isVisibleSideways(i: Int, j: Int, grid: Array[Array[Int]]) =
  grid(i)(j) > grid(i).slice(0, j).reduce(_ max _) ||
    grid(i)(j) > grid(i).slice(j + 1, grid(i).length).reduce(_ max _)

def getScenicScoreSideways(i: Int, j: Int, grid: Array[Array[Int]]) =
  val h = grid(i)(j)
  val leftTrees = grid(i).slice(0, j)
  val rightTrees = grid(i).slice(j + 1, grid(i).length)

  val scoreLeft = {
    val blockIdx = leftTrees.lastIndexWhere(_ >= h)
    if blockIdx >= 0 then
      leftTrees.length - blockIdx
    else
      leftTrees.length
  }
  val scoreRight = {
    val blockIdx = rightTrees.indexWhere(_ >= h)
    if blockIdx >= 0 then
      blockIdx + 1
    else
      rightTrees.length
  }
  scoreLeft * scoreRight

@main def main8 =
  val filename = "input/day8.txt"
  var lines = Source.fromFile(filename).getLines().toList

  val numRows = lines(0).length
  val numCols = lines.length
  val grid = Array.ofDim[Int](numRows, numCols)
  for
    (line, i) <- lines.zipWithIndex
  do
    for
      (height, j) <- line.split("").map(_.toInt).zipWithIndex
    do
      grid(i)(j) = height

  val gridTranspose = grid.transpose

  val visibleInteriorTrees = for
    i <- 1 to numRows - 2
    j <- 1 to numCols - 2
    if isVisibleSideways(i, j, grid) || isVisibleSideways(j, i, gridTranspose)
  yield 1

  val part1 = visibleInteriorTrees.reduce(_ + _) + numCols * 2 + numRows * 2 - 4
  println(s"part 1: $part1")

  // Print the grid for debugging
  // println(grid.map(_.mkString(" ")).mkString("\n"))
  // println(grid.transpose.map(_.mkString(" ")).mkString("\n"))

  val scenicScores = for
    i <- 1 to numRows - 2
    j <- 1 to numCols - 2
  yield getScenicScoreSideways(i, j, grid) * getScenicScoreSideways(j, i, gridTranspose)
  val part2 = scenicScores.reduce(_ max _)
  println(s"part 2 : $part2")

