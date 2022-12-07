package competitive.competitions.aoc.y2022
import competitive.competitions.aoc.AocProblem

case class File(name: String, size: Int)
case class Directory(name: String, var subDirs: Vector[Directory] = Vector.empty, var parent: Option[Directory] = None, var files: Vector[File] = Vector.empty):
    var size = 0
    def addFile(file: File): Unit = 
        files = files :+ file
        size += file.size
        parent.foreach(_.addFile(file))

object Seven extends AocProblem(2022, 7):
    def parse(input: Vector[String]) = 
        val root = Directory("/")
        var dirs = Vector(root)
        var currentDir = root
        input.tail.foreach { line =>
            val prev = currentDir.parent
            if line.startsWith("$ cd") then
                val dirName = line match
                    case s"$$ $cd $dir" => dir
                    case _ => throw new Exception(s"Invalid cd command: $line")
                if dirName == "/" then currentDir = root
                else if dirName == ".." then currentDir = prev.getOrElse(throw new Exception(s"Invalid cd command: $line"))
                else currentDir = currentDir.subDirs.find(_.name == dirName).getOrElse(throw new Exception(s"Invalid cd command: $line"))
            else if line.startsWith("dir") then
                val dirName = line match
                    case s"dir $dir" => dir
                    case _ => throw new Exception(s"Invalid dir command: $line")
                val dir = Directory(dirName, parent = Some(currentDir))
                currentDir.subDirs = currentDir.subDirs :+ dir
                dirs = dirs :+ dir
            else if line(0).isDigit then
                val Array(size, fileName) = line.split(" ")
                val file = File(fileName, size.toInt)
                currentDir.addFile(file)
            else if line != "$ ls" then throw new Exception(s"Invalid command: $line")
        }
        (root, dirs)

        
    override def part1(input: Vector[String]) =
        val (root, dirs) = parse(input)
        dirs.filter(_.size <= 100000).map(_.size).sum.toString

    override def part2(input: Vector[String]) =
        val (root, dirs) = parse(input)
        val usedSize = root.size.toLong
        val maxSize = 70000000L
        val freeSize = maxSize - usedSize
        val neededSize = 30000000L
        dirs.filter(_.size >= neededSize - freeSize).map(_.size).min.toString