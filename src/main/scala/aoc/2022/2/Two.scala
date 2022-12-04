package competitive.aoc.y2022
import competitive.aoc.AocProblem

object Two extends AocProblem(2022, 2):
    private def rps(opponent: String, player: String, oRPS: (String, String, String), pRPS: (String, String, String)) =
        val (oRock, oPaper, oScissors) = oRPS
        val (pRock, pPaper, pScissors) = pRPS
        val (win, lose, draw) = (6, 0, 3)
        (opponent, player) match
            case (`oRock`, `pRock`) | (`oPaper`, `pPaper`) | (`oScissors`, `pScissors`) => draw
            case (`oRock`, `pPaper`) | (`oPaper`, `pScissors`) | (`oScissors`, `pRock`) => win
            case (`oRock`, `pScissors`) | (`oPaper`, `pRock`) | (`oScissors`, `pPaper`) => lose
            case _ => throw new Exception("Invalid input")

    private def score(player: String, pRPS: (String, String, String)) =
        val (rock, paper, scissors) = pRPS
        player match
            case `rock` => 1
            case `paper` => 2
            case `scissors` => 3

    def parse(input: Vector[String]) =
        input.map(_.split(" ")).map(i => (i(0), i(1)))
            

    override def part1(input: Vector[String]) =
        val parsed = parse(input)
        val winScore = parsed.map(i => rps(i._1, i._2, ("A", "B", "C"), ("X", "Y", "Z"))).sum
        val playerScore = parsed.map(_._2).map(score(_, ("X", "Y", "Z"))).sum
        (winScore + playerScore).toString

    private def win(opponent: String) =
        opponent match
            case "A" => "B"
            case "B" => "C"
            case "C" => "A"

    private def lose(opponent: String) =
        opponent match
            case "A" => "C"
            case "B" => "A"
            case "C" => "B"

    override def part2(input: Vector[String]) =
        val parsed = parse(input)
        val (losevar, drawvar, winvar) = ("X", "Y", "Z")
        val (rock, paper, scissors) = ("A", "B", "C")
        val playerScore = 
            parsed.map(p => p._2 match
                    case `losevar` => lose(p._1)
                    case `drawvar` => p._1
                    case `winvar` => win(p._1)
                    case _ => throw new Exception("Invalid input")
                )
                .map(score(_, (rock, paper, scissors)))
                .sum

        val winScore = parsed.map(_._2).map(
            _ match
                case `losevar` => 0
                case `drawvar` => 3
                case `winvar` => 6
                case _ => throw new Exception("Invalid input")
        ).sum

        (winScore + playerScore).toString