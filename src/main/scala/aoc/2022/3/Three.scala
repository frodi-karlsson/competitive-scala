package aoc.y2022
import aoc.Problem

object Three extends Problem(2022, 3):
    val alphabetMap = ('a' to 'z').zipWithIndex.map(p => (p._1, p._2 + 1)).toMap ++ 
        ('A' to 'Z').zipWithIndex.map(i => (i._1, i._2 + 27)).toMap

    override def part1(input: Vector[String]) =
        input
            .map(s => (s.substring(0, s.length / 2), s.substring(s.length / 2)))
            .map(i => 
                i._1.toSet.intersect(i._2.toSet)
            )
            .flatten
            .map(i => alphabetMap(i))
            .sum.toString

    override def part2(input: Vector[String]) =
        input
            .grouped(3)
            .toVector
            .map(i => 
                i(0).toSet.intersect(i(1).toSet).intersect(i(2).toSet)
            )
            .flatten
            .map(i => alphabetMap(i))
            .sum.toString