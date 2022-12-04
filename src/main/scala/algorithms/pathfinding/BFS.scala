package competitive.algorithms.pathfinding

import competitive.algorithms.graphs.*

class BFS[T] extends Pathfinder[T]:
    def findPath(graph: Graph[T, UnweightedEdge[T]], start: Node[T], end: Node[T]): Option[Vector[Node[T]]] =
        val queue = scala.collection.mutable.Queue[Vector[Node[T]]]()
        val visited = scala.collection.mutable.Set[Node[T]]()
        queue.enqueue(Vector(start))
        while queue.nonEmpty do
            val path = queue.dequeue()
            val node = path.last
            if node == end then return Some(path)
            if !visited.contains(node) then
                visited.add(node)
                for edge <- graph.edgesFrom(node) do
                    queue.enqueue(path :+ edge.to)
        None
    
    
    /**
      * finds every path from start to end
      * assumes that the graph is acyclic
      *
      * @param graph
      * @param start
      * @param end
      * @param visitingLogic
      * @return a vector of paths
      */
    def findAllPaths(
        graph: Graph[T, UnweightedEdge[T]], 
        start: Node[T], end: Node[T], 
        visitingLogic: (Node[T], Node[T]) => Boolean = (from, to) => !to.visited,
        onFinishPath: (Node[T], Node[T]) => Unit = (from, to) => from.visited = false
    ): Vector[Vector[Node[T]]] =
        def recurse(curr: Node[T], dest: Node[T], path: Vector[Node[T]], result: Vector[Vector[Node[T]]]): Vector[Vector[Node[T]]] =
            if curr == dest then 
                result :+ path
            else
                curr.visited = true
                var newResult = result
                for edge <- graph.edgesFrom(curr).filter(edge => visitingLogic(curr, edge.to)) do
                        newResult = recurse(edge.to, dest, path :+ edge.to, newResult)
                onFinishPath(curr, dest)
                newResult
        recurse(start, end, Vector(start), Vector())


object TestBFS:
    def testFindPath =
        val nodes = Vector(1, 2, 3, 4).map(Node(_))
        val edges: Vector[UnweightedEdge[Int]] = Vector(
            Edge(nodes(0), nodes(1)),
            Edge(nodes(0), nodes(2)),
            Edge(nodes(1), nodes(2)),
            Edge(nodes(1), nodes(3)),
            Edge(nodes(2), nodes(3))
        )

        val graph = Graph[Int](nodes, edges)
        val pathfinder = BFS[Int]()
        val path = pathfinder.findPath(graph, nodes(0), nodes(3))
        assert(path == Some(Vector(nodes(0), nodes(1), nodes(3))))

    def testFindAllPaths =
        val nodes = Vector(1, 2, 3, 4).map(Node(_))
        val edges: Vector[UnweightedEdge[Int]] = Vector(
            Edge(nodes(0), nodes(1)),
            Edge(nodes(0), nodes(2)),
            Edge(nodes(1), nodes(2)),
            Edge(nodes(2), nodes(1)),
            Edge(nodes(1), nodes(3)),
            Edge(nodes(2), nodes(3))
        )

        val graph = Graph[Int](nodes, edges)
        val pathfinder = BFS[Int]()
        val paths = pathfinder.findAllPaths(graph, nodes(0), nodes(3))
        assert(
            paths.toSet == 
                Set( 
                    Vector(nodes(0), nodes(1), nodes(3)), 
                    Vector(nodes(0), nodes(2), nodes(3)), 
                    Vector(nodes(0), nodes(1), nodes(2), nodes(3)),
                    Vector(nodes(0), nodes(2), nodes(1), nodes(3))
                )
        )

    def test =
        testFindPath
        println("TestBFS.testFindPath passed")
        testFindAllPaths
        println("TestBFS.testFindAllPaths passed")