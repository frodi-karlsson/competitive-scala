package aoc.algorithms.graphs

trait Node[N]:
    val id: N
    var visited: Boolean
    override def toString = s"Node($id, $visited)"
class GenericNode[N](override val id: N, var visited: Boolean = false) extends Node[N]

object Node:
    def apply[N](id: N): Node[N] = new GenericNode(id)