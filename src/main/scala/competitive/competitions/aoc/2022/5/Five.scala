package competitive.competitions.aoc.y2022
import competitive.competitions.aoc.AocProblem
import competitive.algorithms.matrix.*
import competitive.algorithms.language.*

object Five extends AocProblem(2022, 5):
    case class Box(letter: Char, empty: Boolean = false)
    val instruction = Instruction[Unit]("move", 3)

    def parse(input: Vector[String]) = 
        val stacksIndex = input.indexWhere(!_.contains("["))
        val stacks = input(stacksIndex).split("   ").map(_.replaceAll(" ", "").toInt).toVector
        val inputWithoutStacks = input.take(stacksIndex) ++ input.drop(stacksIndex + 1)
        val boxLines = inputWithoutStacks.take(inputWithoutStacks.indexWhere(_.isEmpty))
        val boxes = for line <- boxLines.map(_.zipWithIndex.filter(_._2 % 4 == 1).map(_._1)) yield
                line.map(l => Box(l, l == ' ')).toVector
        val instructions = 
            inputWithoutStacks
                .drop(stacksIndex + 1)
                .map(
                    _.split(" ").toVector
                )
                .map(i => (i(1).toInt, i(3).toInt, i(5).toInt))
        (Matrix(boxes), instructions)

    private def getStr(matrix: Matrix[Box]) = 
        (for col <- 0 until matrix.cols yield
            matrix.col(col).find(!_.empty).get.letter).mkString

    private def execute(execution: (args: Vector[Int]) => Unit, instructions: Vector[(Int, Int, Int)]) =
        for i <- instructions do
            val call = InstructionCall[Unit, Int](instruction, Vector(i._1, i._2, i._3), execution).execute
    
    override def part1(input: Vector[String]) =
        val parsed = parse(input)
        val instructions = parsed._2
        var matrix = parsed._1
        val execution = 
            (args: Vector[Int]) =>
                for i <- 0 until args(0) do
                    val (from, to) = (args(1) - 1, args(2) - 1)
                    if !matrix.col(to).exists(_.empty) then
                        matrix = matrix.withRow(0, matrix.row(0).map(b => Box(' ', true)))
                    matrix = matrix.swapped(
                        (matrix.col(to).zipWithIndex.reverse.find(_._1.empty).get._2, to), 
                        (matrix.col(from).indexWhere(!_.empty), from)
                    )
        execute(execution, instructions)
        getStr(matrix)

    override def part2(input: Vector[String]) =
        val parsed = parse(input)
        val instructions = parsed._2
        var matrix = parsed._1
        val execution = 
            (args: Vector[Int]) =>
                val boxesToTake = matrix.col(args(1) - 1).filter(!_.empty).take(args(0))
                val emptySpaces: Int = matrix.col(args(2) - 1).count(_.empty)
                for i <- 0 until boxesToTake.length - emptySpaces do
                        matrix = matrix.withRow(0, matrix.row(0).map(b => Box(' ', true)))
                for i <- 0 until boxesToTake.length do
                    matrix = matrix.swapped(
                        (matrix.col(args(2) - 1)
                            .zipWithIndex.reverse.find(_._1.empty).get._2 - boxesToTake.length + i + 1, args(2) - 1),
                        (matrix.col(args(1) - 1).indexWhere(!_.empty), args(1) - 1)
                    )
        execute(execution, instructions)
        getStr(matrix)