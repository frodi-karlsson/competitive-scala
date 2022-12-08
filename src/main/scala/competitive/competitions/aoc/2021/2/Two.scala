package competitive.competitions.aoc.y2021
import competitive.competitions.aoc.AocProblem
import competitive.algorithms.language.*

object Two extends AocProblem(2021, 2):
    def parse(input: Vector[String]) = 
        input.map(line =>
            line match
                case s"$dir $steps" => (dir, steps.toInt)
        )

    override def part1(input: Vector[String]) =
        var (x, y) = (0, 0)
        parse(input)
            .foreach((dir, steps) =>
                dir match
                    case "up" => y -= steps
                    case "down" => y += steps
                    case "forward" => x += steps
                    case "backward" => x -= steps
            )
        (x * y).toString

    override def part2(input: Vector[String]) =
        var (x, y, aim) = (0, 0, 0)
        parse(input)
            .foreach((dir, steps) =>
                dir match
                    case "up" => aim -= steps
                    case "down" => aim += steps
                    case "forward" => 
                        x += steps
                        y += steps * aim
                    case "backward" => x -= steps
            )
        (x * y).toString

