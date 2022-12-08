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
                val up = parsed.map(_(c)).take(r).reverse
                val down = parsed.map(_(c)).drop(r + 1)
                val left = parsed(r).take(c).reverse
                val right = parsed(r).drop(c + 1)
        
                val isEdge = r == 0 || r == parsed.size - 1 || c == 0 || c == parsed(0).size - 1
                isEdge || up.forall(_ < node) || down.forall(_ < node) || left.forall(_ < node) || right.forall(_ < node)   
            )
        ).flatten.count(identity).toString

    override def part2(input: Vector[String]) =
        val parsed = parse(input)
        parsed.indices.map(r =>
            parsed(r).indices.map(c =>
                val node = parsed(r)(c)
                val up = parsed.map(_(c)).take(r).reverse
                val down = parsed.map(_(c)).drop(r + 1)
                val left = parsed(r).take(c).reverse
                val right = parsed(r).drop(c + 1)

                val upScore = up.takeWhile(_ < node).size + (if up.exists(_ >= node) then 1 else 0)
                val downScore = down.takeWhile(_ < node).size + (if down.exists(_ >= node) then 1 else 0)
                val leftScore = left.takeWhile(_ < node).size + (if left.exists(_ >= node) then 1 else 0)
                val rightScore = right.takeWhile(_ < node).size + (if right.exists(_ >= node) then 1 else 0)

                upScore * downScore * leftScore * rightScore
            )
        ).flatten.max.toString