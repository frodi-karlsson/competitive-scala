package aoc.algorithms.pathfinding

import aoc.algorithms.graphs.*
import collection.mutable.{Map, Set}

class Dijkstra[T] extends WeightedPathfinder[T]:
    override def findPath(graph: WeightedGraph[T], start: Node[T], end: Node[T]): Option[Vector[Node[T]]] =
        val unvisited = Set[Node[T]]()
        val distances = Map[Node[T], Double]()
        val previous = Map[Node[T], Node[T]]()
        for node <- graph.nodes do
            distances(node) = Double.PositiveInfinity
            previous(node) = null
            unvisited.add(node)
        distances(start) = 0
        while unvisited.nonEmpty do
            val current = unvisited.minBy(distances(_))
            unvisited.remove(current)
            if current == end then
                return Some(reconstructPath(previous, end))
            for neighbour <- graph.getNeighbours(current) do
                val alt = distances(current) + graph.getEdge(current, neighbour).get.weight
                if alt < distances(neighbour) then
                    distances(neighbour) = alt
                    previous(neighbour) = current
        None
    private def reconstructPath(previous: Map[Node[T], Node[T]], end: Node[T]): Vector[Node[T]] =
        var current = end
        var path = Vector[Node[T]]()
        while current != null do
            path = current +: path
            current = previous(current)
        path
        

object TestDijkstra:
    def test =
        val nodes = Vector(1, 2, 3, 4).map(Node(_))
        val edges = Vector(
            Edge(nodes(0), nodes(1), 1),
            Edge(nodes(0), nodes(2), 4),
            Edge(nodes(1), nodes(2), 2),
            Edge(nodes(1), nodes(3), 7),
            Edge(nodes(2), nodes(3), 3)
        )

        val graph = Graph[Int](nodes, edges)
        val pathfinder = Dijkstra[Int]()
        val path = pathfinder.findPath(graph, nodes(0), nodes(3))
        println(s"Path: ${path.mkString(" -> ")}")
        assert(path == Some(Vector(nodes(0), nodes(1), nodes(2), nodes(3))))
        println("Test passed")

