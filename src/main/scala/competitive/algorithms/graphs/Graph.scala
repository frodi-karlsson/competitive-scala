package competitive.algorithms.graphs

trait Graph[N]:
    val nodes: Vector[Node[N]]
    val edges: Vector[Edge[N]]

    def addNode(node: Node[N]): Graph[N]
    def addEdge(edge: Edge[N]): Graph[N]
    def getNeighbours(node: Node[N]): Vector[Node[N]]
    def getEdge(from: Node[N], to: Node[N]): Option[Edge[N]]
    def edgesFrom(node: Node[N]): Vector[Edge[N]]
    def edgesTo(node: Node[N]): Vector[Edge[N]]
    def length: Int
    def pathsOfLength(length: Int): Vector[Vector[Node[N]]]

abstract class GenericGraphImpl[N](val nodes: Vector[Node[N]], val edges: Vector[Edge[N]]) extends Graph[N]:
    def addNode(node: Node[N]): Graph[N] =
        if nodes.contains(node) then this
        else this.getClass.getConstructors()(0).newInstance(nodes :+ node, edges).asInstanceOf[Graph[N]]
    
    def addEdge(edge: Edge[N]): Graph[N] =
        if edges.contains(edge) then this
        else this.getClass.getConstructors()(0).newInstance(nodes, edges :+ edge).asInstanceOf[Graph[N]]

    def getEdge(from: Node[N], to: Node[N]): Option[Edge[N]] =
        edges.find(e => e.from == from && e.to == to)

    def edgesFrom(node: Node[N]): Vector[Edge[N]] =
        edges.filter(_.from == node)

    def edgesTo(node: Node[N]): Vector[Edge[N]] =
        edges.filter(_.to == node)

    def getNeighbours(node: Node[N]): Vector[Node[N]] =
        edgesFrom(node).map(_.to)
    def length = edges.length
    
    def pathsOfLength(length: Int): Vector[Vector[Node[N]]] =
        if length == 0 then Vector(Vector())
        else if length == 1 then nodes.map(Vector(_))
        else
            val paths = pathsOfLength(length - 1)
            paths
                .flatMap(path =>
                    val lastNode = path.last
                    val neighbours = getNeighbours(lastNode).filterNot(path.contains)
                    neighbours.map(n => path :+ n)
                )
                .distinct
    
    override def toString: String = s"Graph($nodes, $edges)"


case class GenericGraph[N](override val nodes: Vector[Node[N]], override val edges: Vector[Edge[N]]) extends GenericGraphImpl[N](nodes, edges)

object Graph:
    def apply[N](nodes: Vector[Node[N]], edges: Vector[Edge[N]]): GenericGraph[N] = GenericGraph(nodes, edges)