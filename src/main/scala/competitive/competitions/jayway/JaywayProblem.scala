package competitive.competitions.jayway

import competitive.competitions.jayway.*
import competitive.util.json.*
import competitive.util.*
import io.circe.*

abstract class JaywayProblem[I](jsonURL: String, year: Int, day: Int) extends JsonProblem[I](Files.fromURL(jsonURL)):
    val basePath = s"./src/main/scala/competitive/competitions/jayway/$year/$day"
    val examplePath = s"$basePath/example"
    val solPath = s"$basePath/sol"

    lazy val example = parse(Files.fromFile(examplePath))
    val sol = Files.fromFile(solPath)
    def test: Unit =
        val exampleSolved = solve(example)
        if exampleSolved == sol then
            println("Example correct")
        else
            println(s"Example incorrect")
            println(s"Expected: $sol")
            println(s"Got: $exampleSolved") 
    def run: String =
        test
        solve(parsed)
    
    def solve(input: List[I]): String

