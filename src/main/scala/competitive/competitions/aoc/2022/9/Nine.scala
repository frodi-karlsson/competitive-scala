package competitive.competitions.aoc.y2022
import competitive.competitions.aoc.AocProblem

object Nine extends AocProblem(2022, 9):
    def parse(input: Vector[String]) = 
        input.map(line =>
            line match
                case s"${dir} ${steps}" => (dir, steps.toInt)
        )

    def moveRope(input: Vector[String], ropeSize: Int) =
        val parsed = parse(input)
        val positionsVisited = collection.mutable.Set[(Int, Int)]()
        val rope = Array.fill(ropeSize)((0, 0))
        positionsVisited.add(rope.last)
        parsed.foreach((dir, steps) =>
            (1 to steps).foreach(step =>
                rope(0) = (
                    rope(0)._1 + (if dir == "R" then 1 else if dir == "L" then -1 else 0), 
                    rope(0)._2 + (if dir == "U" then 1 else if dir == "D" then -1 else 0)
                )
                rope.indices.drop(1).foreach(i =>
                    val (headX, headY) = rope(i-1)
                    val (tailX, tailY) = rope(i)
                    val (yEquals, xEquals) = (headY == tailY, headX == tailX)
                    val (xDiff, yDiff) = (Math.abs(headX - tailX), Math.abs(headY - tailY))
                    val shouldMoveDiagonal = !xEquals && !yEquals && xDiff + yDiff > 2
                    val dx = if xDiff > 1 || shouldMoveDiagonal then if headX > tailX then 1 else -1 else 0
                    val dy = if yDiff > 1 || shouldMoveDiagonal then if headY > tailY then 1 else -1 else 0
                    rope(i) = (tailX + dx, tailY + dy)
                )
                positionsVisited.add(rope.last)
            )
        )
        positionsVisited.size.toString

    override def part1(input: Vector[String]) =
        moveRope(input, 2)
        
    override def part2(input: Vector[String]) =
        moveRope(input, 10)