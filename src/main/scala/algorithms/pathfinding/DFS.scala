package competitive.algorithms.pathfinding

import competitive.algorithms.graphs.*
import collection.mutable.{Set, Stack}

class DFS[T] extends Pathfinder[T]:
    def findPath(graph: Graph[T, UnweightedEdge[T]], start: Node[T], end: Node[T]): Option[Vector[Node[T]]] =
        val stack = Stack[Vector[Node[T]]]()
        val visited = Set[Node[T]]()
        stack.push(Vector(start))
        var count = 0
        while stack.nonEmpty do
            val path = stack.pop()
            val node = path.last
            if node == end then return Some(path)
            if !visited.contains(node) then
                visited.add(node)
                for edge <- graph.edgesFrom(node) do
                    stack.push(path :+ edge.to)
            count += 1
        None        

object TestDFS:
    def test =
        val nodes = Vector(1, 2, 3, 4).map(Node(_))
        val edges: Vector[UnweightedEdge[Int]] = Vector(
            Edge(nodes(0), nodes(1)),
            Edge(nodes(0), nodes(2)),
            Edge(nodes(1), nodes(2)),
            Edge(nodes(1), nodes(3)),
            Edge(nodes(2), nodes(3))
        )

        val graph = Graph[Int](nodes, edges)
        val pathfinder = DFS[Int]()
        val path = pathfinder.findPath(graph, nodes(0), nodes(3))
        assert(path == Some(Vector(nodes(0), nodes(2), nodes(3))))
        println("TestDFS passed")