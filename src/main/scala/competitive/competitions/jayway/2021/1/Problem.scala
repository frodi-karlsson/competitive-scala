package competitive.competitions.jayway.y2021

import competitive.algorithms.matrix.*
import competitive.algorithms.pathfinding.*
import competitive.algorithms.graphs.*
import competitive.util.json.*
import competitive.competitions.jayway.*
import io.circe.*
import io.circe.generic.auto.*

case class Place(name: String, spirit_density: Double, x: Double, y: Double)
object One extends JaywayProblem[Place]("https://knattra.jayway.com/hauntingLocations.json", 2021, 1):
    private def distance(p1: Place, p2: Place): Double = Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))
    private def distance(p1: Place, p2: Place, p3: Place): Double = distance(p1, p2) + distance(p2, p3) + distance(p3, p1)
    override implicit val decoder: io.circe.Decoder[Place] = io.circe.generic.semiauto.deriveDecoder[Place]
    def round(d: Double, places: Int = 3): Double = BigDecimal(d).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble
    def solve(input: List[Place]): String =
        println(s"num lines: ${input.length}")
        val matrix = Matrix(Vector(input.toVector))
        def pathFunc(vec: Vector[Node[(Place, (Int, Int))]]) =
            val path = vec.map(_.id._1)
            round(path.map(_.spirit_density).sum/3) / round(distance(path(0), path(1), path(2)))
        val graph = matrix.toGraph((a, b) => (a.spirit_density + b.spirit_density)/(2 * distance(a, b)) > 0.001)
        val nodes = graph.nodes
        val pathFinder = new BFS[(Place, (Int, Int))]()
        val best = (for 
            start <- nodes
            end <- nodes
            if start != end
        yield
            val path = pathFinder.findAllPaths(graph, start, end, length = 3)
            println(s"start: $start, end: $end, path: $path")
            path
        ).flatten.maxBy(pathFunc)
        round(pathFunc(best)) + " " + best.map(_.id._1.name).mkString(" -> ")
        
            

