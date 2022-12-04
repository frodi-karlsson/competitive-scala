package competitive.competitions.aoc
import scala.io.Source

trait AocProblem(year: Int, day: Int):
    private val basePath = "src/main/scala/competitive/competitions/aoc/"
    private val notImplemented = "Not implemented"
    def part1(input: Vector[String]): String = notImplemented
    def part2(input: Vector[String]): String = notImplemented
    def test: Unit = 
        val input = Source.fromFile(s"./$basePath/$year/$day/test").getLines().toVector
        val input2 = 
            val test2 = new java.io.File(s"./$basePath/$year/$day/test2")
            if test2.exists then
                Source.fromFile(test2).getLines().toVector
            else
                Vector()
        val sol1 = 
            val from = Source.fromFile(s"./$basePath/$year/$day/sol1").getLines()
            if from.hasNext then from.next() else ""
        val sol2 =
            val from = Source.fromFile(s"./$basePath/$year/$day/sol2").getLines()
            if from.hasNext then from.next() else ""

        val p1 = part1(input)
    
        val p2 = part2(if input2.isEmpty then input else input2)

        if p1 == notImplemented then
            println("Part 1: Not implemented")
        else if p1 == sol1 then
            println("Part 1 correct")
        else
            println(s"Part 1 incorrect")
            println(s"Expected: $sol1")
            println(s"Got: $p1")

        if p2 == notImplemented then
            println("Part 2: Not implemented")
        else if p2 == sol2 then
            println("Part 2 correct")
        else
            println(s"Part 2 incorrect")
            println(s"Expected: $sol2")
            println(s"Got: $p2")

    def solve =
        val input = Api(year, day)
        test
        println("Part 1: " + part1(input))
        println("Part 2: " + part2(input))