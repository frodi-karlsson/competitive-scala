package competitive.competitions.aoc.y2022
import competitive.competitions.aoc.AocProblem

object Four extends AocProblem(2022, 4):
    def parse(input: Vector[String]) = 
        input
            .map(_.split(","))
            .map(i => (i(0).split("-"), i(1).split("-")))
            .map(i => (Range.inclusive(i._1(0).toInt, i._1(1).toInt), Range.inclusive(i._2(0).toInt, i._2(1).toInt)))

    override def part1(input: Vector[String]) =
        parse(input)
            .filter(i =>
                i._1.containsSlice(i._2) || i._2.containsSlice(i._1)
            )
            .length.toString

    override def part2(input: Vector[String]) =
         parse(input)
            .filter(i =>
                i._1.exists(i._2.contains)
            )
            .length.toString