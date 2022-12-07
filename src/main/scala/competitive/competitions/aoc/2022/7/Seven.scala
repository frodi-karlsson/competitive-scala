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
        input.tail.foreach(line =>
            line match
                case s"$$ $cd $dir" => 
                    if dir == ".." then currentDir = 
                        currentDir.parent.getOrElse(throw new Exception(s"Invalid cd command: $line"))
                    else currentDir =
                            currentDir.subDirs.find(_.name == dir)
                            .getOrElse(throw new Exception(s"Invalid cd command: $line"))
                case s"dir $dir" => 
                    val directory = Directory(dir, parent = Some(currentDir))
                    currentDir.subDirs = currentDir.subDirs :+ directory
                    dirs = dirs :+ directory
                case "$ ls" =>
                case s"${size} $file" => 
                    val Array(size, fileName) = line.split(" ")
                    val file = File(fileName, size.toInt)
                    currentDir.addFile(file)
                case _ => throw new Exception(s"Invalid cd command: $line")
        )
        (root, dirs)

    override def part1(input: Vector[String]) =
        val (root, dirs) = parse(input)
        dirs.filter(_.size <= 100000).map(_.size).sum.toString

    override def part2(input: Vector[String]) =
        val (root, dirs) = parse(input)
        dirs.filter(_.size >= 30000000L - (70000000L - root.size.toLong)).map(_.size).min.toString