package competitive.competitions.aoc.y2022
import competitive.competitions.aoc.AocProblem
import collection.mutable.Set

object Six extends AocProblem(2022, 6):
    def parse(input: Vector[String]) = 
        input.head

    override def part1(input: Vector[String]) =
        (parse(input).sliding(4).indexWhere(g => g.distinct.length == 4) + 4).toString

    override def part2(input: Vector[String]) =
        (parse(input).sliding(14).indexWhere(g => g.distinct.length == 14) + 14).toString


