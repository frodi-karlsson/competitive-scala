//> using scala "3.2.0"
package aoc.y2022
import aoc.Problem

object One extends Problem(2022, 1):
    def parse(input: Vector[String]) = 
        input
            .foldLeft(Vector(Vector[Int]()))((acc, line) =>
                if line.isEmpty then
                    acc :+ Vector()
                else
                    acc.init :+ (acc.last :+ line.toInt)
            )

    override def part1(input: Vector[String]) =
        parse(input).map(_.sum).max.toString

    override def part2(input: Vector[String]) =
        parse(input).map(_.sum).sorted.reverse.take(3).sum.toString



    