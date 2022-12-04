package competitive.algorithms.pathfinding

import competitive.algorithms.graphs.*

trait Pathfinder[T]

trait WeightedPathfinder[T] extends Pathfinder[T]:
    def findPath(graph: WeightedGraph[T], start: Node[T], end: Node[T]): Option[Vector[Node[T]]]