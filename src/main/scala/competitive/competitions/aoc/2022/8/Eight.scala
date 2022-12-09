package competitive.competitions.aoc.y2022
import competitive.competitions.aoc.AocProblem
import scala.annotation.tailrec

object Eight extends AocProblem(2022, 8):
    def parse(input: Vector[String]) = 
        input.map(_.map(c => c.asDigit).toVector)

    override def part1(input: Vector[String]) =
        val parsed = parse(input)
        parsed.indices.map(r =>
            parsed(r).indices.map(c=>
                val node = parsed(r)(c)
                val (up, down) = parsed.map(_(c)).splitAt(r)
                val (left, right) = parsed(r).splitAt(c)        
                val isEdge = r == 0 || r == parsed.size - 1 || c == 0 || c == parsed(0).size - 1
                isEdge || Vector(up, down.drop(1), left, right.drop(1)).exists(_.forall(_ < node)) 
            )
        ).flatten.count(identity).toString

    override def part2(input: Vector[String]) =
        val parsed = parse(input)
        parsed.indices.map(r =>
            parsed(r).indices.map(c =>
                val node = parsed(r)(c)
                val (up, down) = parsed.map(_(c)).splitAt(r)
                val (left, right) = parsed(r).splitAt(c)
                Seq(up.reverse, down.drop(1), left.reverse, right.drop(1)).map(dir =>
                        dir.takeWhile(_ < node).size + (if dir.exists(_ >= node) then 1 else 0)
                ).product
            )
        ).flatten.max.toString