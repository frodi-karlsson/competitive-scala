package competitive.algorithms.graphs

trait Graph[N, E <: Edge[N]]:
    val nodes: Vector[Node[N]]
    val edges: Vector[E]

    def addNode(node: Node[N]): Graph[N, E]
    def addEdge(edge: E): Graph[N, E]
    def getNeighbours(node: Node[N]): Vector[Node[N]]
    def getEdge(from: Node[N], to: Node[N]): Option[E]
    def edgesFrom(node: Node[N]): Vector[E]
    def edgesTo(node: Node[N]): Vector[E]
    def length: Int

trait UnweightedGraph[N] extends Graph[N, UnweightedEdge[N]]
trait WeightedGraph[N] extends Graph[N, WeightedEdge[N]]
abstract class GenericGraphImpl[N, E <: Edge[N]](val nodes: Vector[Node[N]], val edges: Vector[E]) extends Graph[N, E]:
    def addNode(node: Node[N]): Graph[N, E] =
        if nodes.contains(node) then this
        else this.getClass.getConstructors()(0).newInstance(nodes :+ node, edges).asInstanceOf[Graph[N, E]]

    def addEdge(edge: E): Graph[N, E] =
        if edges.contains(edge) then this
        else this.getClass.getConstructors()(0).newInstance(nodes, edges :+ edge).asInstanceOf[Graph[N, E]]

    def getEdge(from: Node[N], to: Node[N]): Option[E] =
        edges.find(e => e.from == from && e.to == to)

    def edgesFrom(node: Node[N]): Vector[E] =
        edges.filter(_.from == node)

    def edgesTo(node: Node[N]): Vector[E] =
        edges.filter(_.to == node)

    def getNeighbours(node: Node[N]): Vector[Node[N]] =
        edgesFrom(node).map(_.to)
    def length = edges.length
    
    override def toString: String = s"Graph($nodes, $edges)"

case class GenericGraph[N](override val nodes: Vector[Node[N]], override val edges: Vector[UnweightedEdge[N]]) extends GenericGraphImpl[N, UnweightedEdge[N]](nodes, edges) with UnweightedGraph[N]

case class GenericWeightedGraph[N](override val nodes: Vector[Node[N]], override val edges: Vector[WeightedEdge[N]]) extends GenericGraphImpl[N, WeightedEdge[N]](nodes, edges) with WeightedGraph[N]

object Graph:
    def apply[N](nodes: Vector[Node[N]], edges: Vector[UnweightedEdge[N]]): GenericGraph[N] = GenericGraph(nodes, edges)
    def apply[N](nodes: Vector[Node[N]], edges: Vector[WeightedEdge[N]]): GenericWeightedGraph[N] = GenericWeightedGraph(nodes, edges)