package competitive.algorithms.graphs

trait Edge[N]:
    val from: Node[N]
    val to: Node[N]

trait WeightedEdge[N] extends Edge[N]:
    val weight: Double

trait UnweightedEdge[N] extends Edge[N]

case class GenericWeightedEdge[N](override val from: Node[N], override val to: Node[N], override val weight: Double) extends WeightedEdge[N]:
    override def toString = s"WeightedEdge($from, $to, $weight)"

case class GenericEdge[N](override val from: Node[N], override val to: Node[N]) extends UnweightedEdge[N]:
    override def toString = s"Edge($from, $to)"

object Edge:
    def apply[N](from: Node[N], to: Node[N]): UnweightedEdge[N] = GenericEdge(from, to)
    def apply[N](from: Node[N], to: Node[N], weight: Double): WeightedEdge[N] = GenericWeightedEdge(from, to, weight)

