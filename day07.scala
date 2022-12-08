import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

abstract class TreeNode

class File(var parent: DirectoryTree, val size: Int) extends TreeNode

class DirectoryTree(var parent: DirectoryTree) extends TreeNode:
  val children: Map[String, TreeNode] = Map.empty[String, TreeNode]

  def addChild(newChild: (String, TreeNode)): Unit =
    children += newChild

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
          }
          currentDir.addChild(newChild)
      }

    i += 1

// Traverse the dir tree to recursively compute the dir size
// Also save all dir sizes into a List
def getDirSize(tree: DirectoryTree, allDirSizes: ListBuffer[Int]): Int =
  val contentSizes = tree.children.values.map {
    case d: DirectoryTree => getDirSize(d, allDirSizes)
    case f: File => f.size
  }
  val totalSize = contentSizes.reduce(_ + _)
  allDirSizes.append(totalSize)
  totalSize

@main def main7() =
  val filename = "input/day07.txt"
  val rootDir = DirectoryTree(null)

  buildDirTreeFromInput(filename, rootDir)

  val allDirSizes = ListBuffer[Int]()
  val rootDirSize = getDirSize(rootDir, allDirSizes)
  val part1 = allDirSizes.filter(_ < 100_000).reduce(_ + _)
  println(s"part 1: $part1")

  val availSpace = 70_000_000 - rootDirSize
  val part2 = allDirSizes.filter(availSpace + _ >= 30_000_000).reduce(_ min _)
  println(s"part 2: $part2")
