package competitive.competitions.aoc.y2021
import competitive.competitions.aoc.AocProblem

object One extends AocProblem(2021, 1):
    def parse(input: Vector[String]) = 
        input.map(_.toInt)

    override def part1(input: Vector[String]) =
        parse(input).sliding(2).filter(v => v(0) < v(1)).size.toString

    override def part2(input: Vector[String]) =
        parse(input).sliding(3).map(_.sum).sliding(2).filter(v => v(0) < v(1)).size.toString

