package competitive.algorithms.pathfinding

import competitive.algorithms.graphs.*

trait Pathfinder[T]:
    def findPath(graph: Graph[T], start: Node[T], end: Node[T]): Option[Vector[Node[T]]]