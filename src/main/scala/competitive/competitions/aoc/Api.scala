package competitive.competitions.aoc
import competitive.util.Files

object Api:
    def url(year: Int, day: Int) = s"https://adventofcode.com/$year/day/$day/input"
    def getInput(year: Int, day: Int, inputNr: Int) =
        val url = Api.url(year, day) + (if inputNr > -1 then s"/$inputNr" else "")
        val token = Files.fromFile("./token")
        val header = ("Cookie", s"session=$token")
        Files.fromURL(url, header)

    def apply(year: Int, day: Int, inputNr: Int = -1) = getInput(year, day, inputNr).split("\n").map(_.replaceAll("\r", "")).toVector
