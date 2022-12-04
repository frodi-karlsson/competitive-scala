package competitive.algorithms.graphs

trait Edge[N]:
    val from: Node[N]
    val to: Node[N]
    val weight: Double

case class GenericEdge[N](override val from: Node[N], override val to: Node[N], override val weight: Double) extends Edge[N]:
    override def toString = s"Edge($from, $to, $weight)"

object Edge:
    def apply[N](from: Node[N], to: Node[N], weight: Double = 0): Edge[N] = GenericEdge(from, to, weight)

