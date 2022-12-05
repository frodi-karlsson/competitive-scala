package competitive.algorithms.matrix
import competitive.algorithms.graphs.*

enum ROT:
    case cw, ccw
enum POS:
    case top, bottom, left, right
enum FLIP:
    case x, y

trait MatrixTrait[T]:
    protected val vector: Vector[Vector[T]]
    /**
      * Returns the number of rows in the matrix
      */
    val rows: Int
    /**
      * Returns the number of columns in the matrix
      */
    val cols: Int
    /**
      * Returns the element at the given row and column
      * @param row The row of the element
      * @param col The column of the element
      * @return The element at the given row and column
      */
    def apply(row: Int, col: Int): T
    /**
      * Returns the row at the given index
      * @param row The index of the row
      * @return The row at the given index
      */
    def row(row: Int): Vector[T]
    /**
      * Returns the column at the given index
      * @param col The index of the column
      * @return The column at the given index
      */
    def col(col: Int): Vector[T]
    /**
     * Returns an updated matrix with the given element at the given row and column
     * @param row The row of the element
     * @param col The column of the element
     * @param elem The element to be placed at the given row and column
     * @return The updated matrix
     */
    def updated(row: Int, col: Int, value: T): Matrix[T]
    /**
     * Returns a mappped matrix with the given function applied to each element
     * @param f The function to be applied to each element
     * @return The mapped matrix
     */
    def map[U](f: T => U): Matrix[U]
    /**
     * Executes the given function on each element
     * @param f The function to be executed on each element
     */
    def foreach(f: T => Unit): Unit
    /**
     * Returns the vector representation of the matrix
     * @return The vector representation of the matrix
     */
    def toVector: Vector[Vector[T]]
    /**
     * Returns the string representation of the matrix
     * @return The string representation of the matrix
     */
    def toString: String
    /**
     * Returns the matrix rotated clockwise or counter-clockwise
     * @param rot The rotation direction
     * @param times The number of times to rotate
     * @return The rotated matrix
     */
    def rotate(rot: ROT, times: Int = 1): Matrix[T]
    /**
     * Returns the matrix flipped horizontally or vertically
     * @param flip The flip direction
     * @return The flipped matrix
     */
    def flip(flip: FLIP): Matrix[T]
    /**
      * Transposes the matrix
      * @return The transposed matrix
      */
    def transpose: Matrix[T]
    /**
      * Returns the matrix zippered with the given matrix
      * @param other The matrix to be zippered with
      * @return The zippered matrix
      */
    def zip[U](other: Matrix[U]): Matrix[(T, U)]
    /**
      * Returns the matrix with the given matrix added to it
      * @param other The matrix to be added to the matrix
      * @param pos The position of the other matrix, default is bottom
      * @return The matrix with the other matrix added to it
      */
    def +(other: Matrix[T], pos: POS = POS.bottom): Matrix[T]
    /**
      * Returns the matrix with the given row added to it
      * @param row Where to add the row, with 0 being the top and rows being the bottom
      * @param values The row to be added to the matrix
      * @return The matrix with the row added to it
      */
    def withRow(row: Int, values: Vector[T]): Matrix[T]
    /**
      * Returns the matrix with the given column added to it
      * @param col Where to add the column, with 0 being the left and cols being the right
      * @param values The column to be added to the matrix
      * @return The matrix with the column added to it
      */
    def withCol(col: Int, values: Vector[T]): Matrix[T]
    /**
      * Returns the matrix with the given row removed
      * @param row The row to be removed
      * @return The matrix with the row removed
      */
    def withoutRow(row: Int): Matrix[T]
    /**
      * Returns the matrix with the given column removed
      * @param col The column to be removed
      * @return The matrix with the column removed
      */
    def withoutCol(col: Int): Matrix[T]
    /**
      * Returns the matrix with values at the given positions swapped
      *
      * @param pos1 The first position
      * @param pos2 The second position
      * @return The matrix with values at the given positions swapped
      */
    def swapped(pos1: (Int, Int), pos2: (Int, Int)): Matrix[T]
    /**
      * Returns a vector containing a range of rows
      * @param 
      * @param start The start of the range
      * @param end The end of the range
      * @return A vector containing a range of rows
      */
    def rowRange(start: Int, end: Int): Vector[Vector[T]]
    /**
      * Returns a vector containing a range of columns
      * @param start The start of the range
      * @param end The end of the range
      * @return A vector containing a range of columns
      */
    def colRange(start: Int, end: Int): Vector[Vector[T]]
    /**
      * Constructs a graph from the matrix
      * @param f The function to dermine if there is an edge between two elements
      * @param w The function to determine the weight of an edge
      * @return A graph constructed from the matrix
      */
    def toGraph(f: (T, T) => Boolean, w: (T, T) => Double): Graph[(T, (Int, Int))]
    /**
      * Constructs a graph from the matrix using indices for f
      * @param f The function to dermine if there is an edge between two elements
      * @param w The function to determine the weight of an edge
      * @return A graph constructed from the matrix
      */
    def toGraphIndexLogic(f: ((Int, Int), (Int, Int)) => Boolean, w: (T, T) => Double): Graph[(T, (Int, Int))]
    /**
      * Constructs a graph from the matrix using indices for w
      * @param f The function to dermine if there is an edge between two elements
      * @param w The function to determine the weight of an edge
      * @return A graph constructed from the matrix
      */
    def toGraphIndexWeight(f: (T, T) => Boolean, w: ((Int, Int), (Int, Int)) => Double): Graph[(T, (Int, Int))]
    /**
      * Constructs a graph from the matrix using indices for f and w
      * @param f The function to dermine if there is an edge between two elements
      * @param w The function to determine the weight of an edge
      * @return A graph constructed from the matrix
      */
    def toGraphOnlyIndices(f: ((Int, Int), (Int, Int)) => Boolean, w: ((Int, Int), (Int, Int)) => Double): Graph[(T, (Int, Int))]
    def isRectangular: Boolean
    def isSquare: Boolean

case class Matrix[T](override protected val vector: Vector[Vector[T]] = Vector.empty) extends MatrixTrait[T]:
    override val rows: Int = vector.length
    override val cols: Int = if rows > 0 then vector(0).length else 0
    assert(isRectangular, "Matrix must be rectangular")
    override def apply(row: Int, col: Int): T = vector(row)(col)
    override def row(row: Int): Vector[T] = vector(row)
    override def col(col: Int): Vector[T] = vector.map(_(col))
    override def updated(row: Int, col: Int, value: T): Matrix[T] = Matrix(vector.updated(row, vector(row).updated(col, value)))
    override def map[U](f: T => U): Matrix[U] = Matrix(vector.map(_.map(f)))
    override def foreach(f: T => Unit): Unit = vector.foreach(_.foreach(f))
    override def toVector: Vector[Vector[T]] = vector
    override def toString: String = vector.map(_.mkString(" ")).mkString("\n")
    override def rotate(rot: ROT, times: Int = 1): Matrix[T] = rot match
        case ROT.cw => 
            var vec = vector
            for i <- 0 to times do
                vec = vec.transpose.map(_.reverse)
            Matrix(vec)
        case ROT.ccw =>
            var vec = vector
            for i <- 0 to times do
                vec = vec.reverse.transpose
            Matrix(vec)
    override def flip(flip: FLIP): Matrix[T] = flip match
        case FLIP.x => Matrix(vector.map(_.reverse))
        case FLIP.y => Matrix(vector.reverse)
    override def transpose: Matrix[T] = Matrix(vector.transpose)
    override def zip[U](other: Matrix[U]): Matrix[(T, U)] = Matrix(vector.zip(other.vector).map(v => v._1.zip(v._2)))
    override def +(other: Matrix[T], pos: POS = POS.bottom): Matrix[T] = pos match
        case POS.top => Matrix(other.vector ++ vector)
        case POS.bottom => Matrix(vector ++ other.vector)
        case POS.left => Matrix(vector.zip(other.vector).map((row, otherRow) => otherRow ++ row))
        case POS.right => Matrix(vector.zip(other.vector).map((row, otherRow) => row ++ otherRow))
    // adds the row at the given index, prepending if index is 0 and appending if index is rows
    override def withRow(row: Int, values: Vector[T]): Matrix[T] =
        if row == 0 then Matrix(values +: vector)
        else if row == rows then Matrix(vector :+ values)
        else Matrix(vector.take(row) ++ Vector(values) ++ vector.drop(row))
    override def withCol(col: Int, values: Vector[T]): Matrix[T] =
        if col == 0 then Matrix(vector.map(c => values.head +: c).zip(values.tail).map((row, value) => value +: row))
        else if col == cols then Matrix(vector.map(c => c :+ values.head).zip(values.tail).map((row, value) => row :+ value))
        else Matrix(vector.zip(values).map((row, value) => row.take(col) ++ Vector(value) ++ row.drop(col)))
    override def withoutRow(row: Int): Matrix[T] = 
        if row < 0 || row >= rows then this
        else Matrix(vector.take(row) ++ vector.drop(row + 1))
    override def withoutCol(col: Int): Matrix[T] =
        if col < 0 || col >= cols then this
        else Matrix(vector.map(row => row.take(col) ++ row.drop(col + 1)))
    override def swapped(pos1: (Int, Int), pos2: (Int, Int)): Matrix[T] = 
        val (row1, col1) = pos1
        val (row2, col2) = pos2
        val value1 = apply(row1, col1)
        val value2 = apply(row2, col2)
        updated(row1, col1, value2).updated(row2, col2, value1)
    override def rowRange(start: Int, end: Int): Vector[Vector[T]] = vector.slice(start, end)
    override def colRange(start: Int, end: Int): Vector[Vector[T]] = vector.map(_.slice(start, end))
    private val nodes =
         vector
            .map(_.zipWithIndex)
            .zipWithIndex
            .flatMap((row, i) => row.map((value, j) => (value, (i, j))))
            .map((value, pos) => Node(value, pos))
    override def toGraph(
        f: (T, T) => Boolean = (_, _) => true, 
        w: (T, T) => Double = (_, _) => 1.0
    ): Graph[(T, (Int, Int))] =
        val edges = (for
                node <- nodes
                node2 <- nodes
                if f(node.id._1, node2.id._1) && node.id._2 != node2.id._2
            yield
                Edge(node, node2, w(node.id._1, node2.id._1))
        ).toVector
        Graph(nodes, edges)
    override def toGraphIndexLogic(
        f: ((Int, Int), (Int, Int)) => Boolean = (_, _) => true, 
        w: (T, T) => Double = (_, _) => 1.0
    ): Graph[(T, (Int, Int))] =
        val edges = (for
                node <- nodes
                node2 <- nodes
                if f(node.id._2, node2.id._2) && node.id._1 != node2.id._1
            yield
                Edge(node, node2, w(node.id._1, node2.id._1))
        ).toVector
        Graph(nodes, edges)
    override def toGraphIndexWeight(
        f: (T, T) => Boolean = (_, _) => true, 
        w: ((Int, Int), (Int, Int)) => Double = (_, _) => 1.0
    ): Graph[(T, (Int, Int))] =
        val edges = (for
                node <- nodes
                node2 <- nodes
                if f(node.id._1, node2.id._1) && node.id._2 != node2.id._2
            yield
                Edge(node, node2, w(node.id._2, node2.id._2))
        ).toVector
        Graph(nodes, edges)

    override def toGraphOnlyIndices(
        f: ((Int, Int), (Int, Int)) => Boolean = (_, _) => true, 
        w: ((Int, Int), (Int, Int)) => Double = (_, _) => 1.0
    ): Graph[(T, (Int, Int))] =
        val edges = (for
                node <- nodes
                node2 <- nodes
                if f(node.id._2, node2.id._2) && node.id._1 != node2.id._1
            yield
                Edge(node, node2, w(node.id._2, node2.id._2))
        ).toVector
        Graph(nodes, edges)
    
    
    override def isRectangular: Boolean = vector.forall(_.length == cols)
    override def isSquare: Boolean = rows == cols

object Matrix:
    def apply[T](rows: Int, cols: Int, f: (Int, Int) => T): Matrix[T] = Matrix(Vector.tabulate(rows, cols)(f))
    def apply[T](rows: Int, cols: Int, value: T): Matrix[T] = Matrix(Vector.fill(rows, cols)(value))
    def apply[T](rows: Int, cols: Int): Matrix[T] = Matrix(Vector.fill(rows, cols)(null.asInstanceOf[T]))

    def testRows: Unit = 
        val mat = Matrix(3, 3, (row, col) => row)
        assert(mat.rows == 3)
    def testCols: Unit = 
        val mat = Matrix(3, 3, (row, col) => col)
        assert(mat.cols == 3)
    def testApply: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat(0, 0) == 0)
        assert(mat(0, 1) == 0)
        assert(mat(0, 2) == 0)
        assert(mat(1, 0) == 0)
        assert(mat(1, 1) == 1)
        assert(mat(1, 2) == 2)
        assert(mat(2, 0) == 0)
        assert(mat(2, 1) == 2)
        assert(mat(2, 2) == 4)
    def testRow: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.row(1) == Vector(0, 1, 2))
    def testCol: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.col(1) == Vector(0, 1, 2))
    def testUpdated: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.updated(0, 0, 1)(0, 0) == 1)
    def testMap: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.map(_ + 1)(0, 0) == 1)
    def testForeach: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        var sum = 0
        mat.foreach(sum += _)
        assert(sum == 9)
    def testToVector: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.toVector == Vector(Vector(0, 0, 0), Vector(0, 1, 2), Vector(0, 2, 4)))
    def testRotate: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.rotate(ROT.cw)(2, 2) == 0)
        assert(mat.rotate(ROT.ccw)(0, 1) == 2)
    def testFlip: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.flip(FLIP.x)(1, 0) == 2)
        assert(mat.flip(FLIP.y)(0, 1) == 2)
    def testTranspose: Unit =
        val vec = Vector(Vector(0, 1, 2), Vector(3, 4, 5), Vector(6, 7, 8))
        val mat = Matrix(vec)
        assert(mat.transpose.toVector == vec.transpose)
    def testZip: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        val other = Matrix(3, 3, (row, col) => row + col)
        assert(mat.zip(other)(0, 0) == (0, 0))
        assert(mat.zip(other)(0, 1) == (0, 1))
        assert(mat.zip(other)(0, 2) == (0, 2))
        assert(mat.zip(other)(1, 0) == (0, 1))
        assert(mat.zip(other)(1, 1) == (1, 2))
    def testWithRow: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.withRow(0, Vector(1, 1, 1))(0, 0) == 1)
    def testWithCol: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.withCol(0, Vector(1, 1, 1))(0, 0) == 1)
    def testWithoutRow: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.withoutRow(0).rows == 2)
    def testWithoutCol: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.withoutCol(0).cols == 2)
    def testSwapped: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.swapped((0, 0), (1, 1))(0, 0) == 1)
    def testRowRange: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.rowRange(0, 2) == Vector(Vector(0, 0, 0), Vector(0, 1, 2)))
    def testColRange: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.colRange(0, 2) == Vector(Vector(0, 0), Vector(0, 1), Vector(0, 2)))
    def testToGraph: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        val graph = mat.toGraph((a, b) => a == b, (a, b) => 1)
        val edges = graph.edges.map(e => (e.from.id, e.to.id, e.weight))
        assert(edges.toSet == Set((0, 0, 1.0), (2, 2, 1.0)) && graph.nodes.size == 9 && graph.edges.size == 22)
    def testToGraphIndexLogic: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        val graph = mat.toGraphIndexLogic((p1, p2) => p1._1 == p2._1, (p1, p2) => 1)
        val edges = graph.edges.map(e => (e.from.id, e.to.id, e.weight))
        assert(edges.toSet == Set((0,2,1.0), (0,4,1.0), (1,0,1.0), (0,0,1.0), (2,0,1.0), (2,1,1.0), (0,1,1.0), (4,0,1.0), (2,4,1.0), (1,2,1.0), (4,2,1.0)) && graph.nodes.size == 9 && graph.edges.size == 18)
    def testToGraphIndexWeight: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        val graph = mat.toGraphIndexWeight((a, b) => a == b && a == 2, (p1, p2) => p1._1 + p2._1)
        val edges = graph.edges.map(e => (e.from.id, e.to.id, e.weight))
        assert(edges.toSet == Set((2, 2, 3.0)) && graph.nodes.size == 9 && graph.edges.size == 2)
    def testToGraphOnlyIndices: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        val graph = mat.toGraphOnlyIndices((p1, p2) => p1._1 == p2._1, (p1, p2) => 1)
        val edges = graph.edges.map(e => (e.from.id, e.to.id, e.weight))
        assert(edges.toSet == Set((0,2,1.0), (0,4,1.0), (1,0,1.0), (0,0,1.0), (2,0,1.0), (2,1,1.0), (0,1,1.0), (4,0,1.0), (2,4,1.0), (1,2,1.0), (4,2,1.0)) && graph.nodes.size == 9 && graph.edges.size == 18)
    def testIsRectangular: Unit =
        val mat = Matrix(3, 4, (row, col) => row * col)
        assert(mat.isRectangular)
    def testIsSquare: Unit =
        val mat = Matrix(3, 3, (row, col) => row * col)
        assert(mat.isSquare)
    def test: Unit =
        println("Testing Matrix")
        testRows
        println("testRows passed")
        testCols
        println("testCols passed")
        testApply
        println("testApply passed")
        testRow
        println("testRow passed")
        testCol
        println("testCol passed")
        testUpdated
        println("testUpdated passed")
        testMap
        println("testMap passed")
        testForeach
        println("testForeach passed")
        testToVector
        println("testToVector passed")
        testRotate
        println("testRotate passed")
        testFlip
        println("testFlip passed")
        testTranspose
        println("testTranspose passed")
        testZip
        println("testZip passed")
        testWithRow
        println("testWithRow passed")
        testWithCol
        println("testWithCol passed")
        testWithoutRow
        println("testWithoutRow passed")
        testWithoutCol
        println("testWithoutCol passed")
        testSwapped
        println("testSwapped passed")
        testRowRange
        println("testRowRange passed")
        testColRange
        println("testColRange passed")
        // testToGraph
        // println("testToGraph passed")
        // testToGraphIndexLogic
        // println("testToGraphIndexLogic passed")
        // testToGraphIndexWeight
        // println("testToGraphIndexWeight passed")
        // testToGraphOnlyIndices
        // println("testToGraphOnlyIndices passed")
        testIsRectangular
        println("testIsRectangular passed")
        testIsSquare
        println("testIsSquare passed")



