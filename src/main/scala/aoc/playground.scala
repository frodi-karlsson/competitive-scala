package aoc

import aoc.algorithms.matrix.*
import aoc.algorithms.graphs.*
import aoc.algorithms.pathfinding.*

//@main
def test =
    val example = """
OU-xt
hq-xt
br-HP
WD-xt
end-br
start-OU
hq-br
MH-hq
MH-start
xt-br
end-WD
hq-start
MH-br
qw-OU
hm-WD
br-WD
OU-hq
xt-MH
qw-MH
WD-qw
end-qw
qw-xt"""
            .stripMargin
            .linesIterator
            .toVector
            .map(_.split("-").toVector) // now it is a Vector[Vector[String]]
            .map(_.map(_.trim))
            .map(_.filter(_.nonEmpty))
            .filter(_.nonEmpty)
    val nodes = 
        example
            .map(_.map(n => (n, Node(n))))
            .map(_.toVector)
            .toVector
            .flatten
            .toMap
    val edgesTmp =
        example
            .map(_.map(nodes(_)))
    val edges =
        edgesTmp
            .map(e => Vector(Edge(e(0), e(1)), Edge(e(1), e(0))))
            .flatten
            .distinctBy(e => (e.from, e.to))
            .toVector
    val graph = Graph[String](nodes.values.toVector, edges)
    val pathfinder = BFS[String]()
    val visitCounter = scala.collection.mutable.Map[Node[String], Int]()
    nodes.values.foreach(n => visitCounter(n) = 0)
    def condition(from: Node[String], to: Node[String]) =
        val condition = 
            (
                !(
                    (visitCounter(to) >= 2) 
                    && to.id.forall(_.isLower)
                )
            ) 
            && to.id != "start" 
            && from.id != "end"

        if condition then
            visitCounter(to) = visitCounter.getOrElse(to, 0) + 1
        condition

    def onFinishPath(from: Node[String], to: Node[String]) =
        visitCounter(from) = visitCounter.getOrElse(from, 1) - 1

    val paths = 
        pathfinder.findAllPaths(
            graph, 
            nodes("start"), 
            nodes("end"), 
            condition,
            onFinishPath
        )
    println(s"Paths: ${paths.size}")
    