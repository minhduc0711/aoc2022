import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

abstract class TreeNode:
  def getSize: Int

class File(var parent: DirectoryTree, val size: Int) extends TreeNode:
  def getSize: Int = size

class DirectoryTree(var parent: DirectoryTree) extends TreeNode:
  val children: Map[String, TreeNode] = Map.empty[String, TreeNode]

  def getSize: Int = children.values.map(_.getSize).reduce(_ + _)

  def addChildren(newChildren: List[(String, TreeNode)]): Unit =
    children ++= newChildren

  def addChild(newChild: (String, TreeNode)): Unit =
    children += newChild

  // override def toString(): String = children.mkString

def buildDirTreeFromInput(filename: String, rootDir: DirectoryTree) = 
  var lines = Source.fromFile(filename).mkString.split("\n")

  var currentDir: DirectoryTree = null
  var i = 0
  while i < lines.length do
    // println(s"i=$i")
    // println(lines(i))
    lines(i) match
      // Handle the cd command
      case "$ cd /" => currentDir = rootDir
      case "$ cd .." => currentDir = currentDir.parent
      case s"$$ cd $dst" => currentDir = currentDir.children(dst) match {
        case d: DirectoryTree => d
        case f: File => throw new Exception(s"cd: not a directory: $dst")
        case _ => throw new Exception(s"cd: no such file or directory: $dst")
      }
      // Handle the ls command
      case "$ ls" => {
        // Scan ahead for output of ls, stop scanning when find a new command
        while
          i + 1 < lines.length && !lines(i + 1).startsWith("$")
        do
          i += 1
          // Parse each output line and add to current dir's content
          val newChild: (String, TreeNode) = lines(i) match {
            case s"dir $name" =>
              (name, DirectoryTree(currentDir))
            case s"$size $name" => (name, File(currentDir, size.toInt))
            case _ => throw new Exception("Can't parse this ls output")
          }
          currentDir.addChild(newChild)
      }
      case _ => None

    i += 1

// Traverse the dir tree to recursively compute the dir size
// For part 1: also keep track of directories with size < 100000
def getDirSize(tree: DirectoryTree, satisfiedSizes: ListBuffer[Int]): Int =
  val contentSizes = tree.children.values.map {
    case d: DirectoryTree => getDirSize(d, satisfiedSizes)
    case f: File => f.size
  }
  val totalSize = contentSizes.reduce(_ + _)
  if totalSize <= 100000 then
    satisfiedSizes.append(totalSize)
  totalSize

// Traverse the dir tree to recursively compute the dir sizes
// For part 2: also keep track of deletable directories s.t. new unused space will be >= 3*10^7
def getDirSize(tree: DirectoryTree, satisfiedSizes: ListBuffer[Int], unusedSpaceSize: Int): Int =
  val contentSizes = tree.children.values.map {
    case d: DirectoryTree => getDirSize(d, satisfiedSizes, unusedSpaceSize)
    case f: File => f.size
  }
  val totalSize = contentSizes.reduce(_ + _)
  if (unusedSpaceSize + totalSize) >= 30000000 then
    satisfiedSizes.append(totalSize)
  totalSize

@main def main7() =
  val filename = "input/day7.txt"
  val rootDir = DirectoryTree(null)

  buildDirTreeFromInput(filename, rootDir)

  val part1Sizes = ListBuffer[Int]()
  val rootDirSize = getDirSize(rootDir, part1Sizes)
  val part1 = part1Sizes.reduce(_ + _)
  println(s"part 1: $part1")

  val part2Sizes = ListBuffer[Int]()
  getDirSize(rootDir, part2Sizes, 70000000 - rootDirSize)
  val part2 = part2Sizes.reduce(_ min _)
  println(s"part 2: $part2")
