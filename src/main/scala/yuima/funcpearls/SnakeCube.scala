package yuima.funcpearls

/** Scala implementation of "Solving the Snake Cube Puzzle in Haskel."
  * http://web.cecs.pdx.edu/~mpj/snakecube/revised-SnakeCube.pdf
  *
  * Note: No implementation for a function "advance" in section 9.
  *
  * @author Yuichiroh Matsubayashi
  *         Created on 14/11/28.
  */
object SnakeCube {
  type Section = List[Position]
  type Solution = List[Section]

  val snake = List(3, 2, 2, 3, 2, 3, 2, 2, 3, 3, 2, 2, 2, 3, 3, 3, 3)

  val standard = SnakeCubePuzzle(
    sections = snake,
    valid = inCube(3)(_),
    initialSolution = List(List(Position(1, 1, 1))),
    initialDirection = Direction(0, 0, 1)
  )

  val meanGreen = standard.copy(sections = List(3, 3, 2, 3, 2, 3, 2, 2, 2, 3, 3, 3, 2, 3, 3, 3))

  val king = standard.copy(valid = inCube(4)(_), sections = List(
    3, 2, 3, 2, 2, 4, 2, 3, 2, 3, 2, 3, 2, 2, 2,
    2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 3, 4, 2,
    2, 2, 4, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 2)
  )

  val king1 = king.copy(initialSolution = List(List(Position(2, 1, 1))))
  val king2 = king.copy(initialSolution = List(List(Position(1, 2, 2))))

  /** Colors used for visualization.
    * Cubes having an odd index are brown and even index are white. */
  val colors = Iterator.continually(Seq("brown", "white")).flatten

  def main(args: Array[String]) {
    solutions(standard).foreach(println)
  }

  /** gets solutions of a puzzle using a brute force algorithm. */
  def solutions(p: SnakeCubePuzzle) = {
    def solve(solution: Solution)(prevDir: Direction)(sections: List[Int]): List[Solution] = sections match {
      case Nil => List(solution)
      case length :: secs =>
        newDirections(prevDir).flatMap { newDir =>
          extend(p)(solution)(newDir)(length).flatMap { newSolution => solve(newSolution)(newDir)(secs) }
        }
    }

    solve(p.initialSolution)(p.initialDirection)(p.sections)
  }

  /** gets possible directions for a next move. */
  def newDirections(prev: Direction) = List(
    Direction(prev.v, prev.w, prev.u),
    Direction(-prev.v, -prev.w, -prev.u),
    Direction(prev.w, prev.u, prev.v),
    Direction(-prev.w, -prev.u, -prev.v)
  )

  /** extends current move sequences with a move toward a particular direction.
    * Note: we must ensure that all of the positions in next section are valid in the given puzzle,
    * and we must also check that none of the positions in next section have already been occupied by
    * other sections in the starting solution.
    * @param p        puzzle definition
    * @param solution the move sequence so far
    * @param dir      the direction of a next move
    * @param length   the cube length of the next section
    **/
  def extend(p: SnakeCubePuzzle)(solution: Solution)(dir: Direction)(length: Int) = {
    val start = solution.head.head
    val nextSec = section(start)(dir)(length)
    if (nextSec.forall(p.valid) && solution.forall(sec => (nextSec intersect sec).isEmpty)) List(nextSec :: solution)
    else Nil
  }

  /** obtains small-cube positions for a next move.
    * @param start  the beginning position of the move.
    * @param d      the direction of the move.
    * @param length the small-cube length of the move.
    * */
  def section(start: Position)(d: Direction)(length: Int) = {
    def pieces = Stream.iterate(start) { pos: Position => Position(pos.x + d.u, pos.y + d.v, pos.z + d.w) }
    pieces.take(length).tail.reverse.toList
  }

  /** creates a variant of a puzzle by reversing the order of the sections. */
  def reversePuzzle(p: SnakeCubePuzzle) = p.copy(sections = p.sections.reverse)

  /** checks whether a given cube location is valid for a solution (it should be inside of the large cube).
    * @param size the cube size of the solution (i.e. the cube is size * size * size)
    * @param pos  the position of a target cube.
    **/
  def inCube(size: Int)(pos: Position) = {
    def inRange(k: Int) = 1 <= k && k <= size

    inRange(pos.x) && inRange(pos.y) && inRange(pos.z)
  }

  /** converts an objective of the puzzle to a different one --that is to find the most-compact,
    * flat form where all of the sections in a single level. (i.e. z == 1 for all cubes) */
  def flatPuzzle(p: SnakeCubePuzzle) = p.copy(valid = (pos: Position) => pos.z == 1)

  /** returns sketch format data representing a one of the solution. */
  def showSteps(p: SnakeCubePuzzle) = steps(solutions(p).head).map(showCubes).mkString("\n")

  def steps(solution: Solution) = solution.map(_.reverse).reverse.tail

  /** returns polygon info of cubes for the sketching tool.
    * @param positions the cube positions
    **/
  def showCubes(positions: List[Position]) = positions.map(p => cube2sketch(colors.next)(p)).mkString("\n")

  /** returns polygon info of a single cube for the sketching tool. */
  def cube2sketch(color: String)(a: Position) = {
    val prefix = s"polygon[fill=$color]"
    val Position(x, y, z) = a
    val b = (x, y, z - 1)
    val c = (x, y - 1, z)
    val d = (x - 1, y, z)
    val e = (x, y - 1, z - 1)
    val f = (x - 1, y, z - 1)
    val g = (x - 1, y - 1, z)
    val h = (x - 1, y - 1, z - 1)
    val faces = List(List(a, d, g, c), List(b, e, h, f), List(a, b, f, d), List(c, g, h, e), List(a, c, e, b), List(d, f, h, g))

    faces.map(f => prefix + f.mkString("")).mkString("\n")
  }

  /** SnakeCube puzzle */
  case class SnakeCubePuzzle(sections: List[Int],
                             valid: Position => Boolean,
                             initialSolution: Solution,
                             initialDirection: Direction)

  case class Direction(u: Int, v: Int, w: Int) {
    require(u.abs + v.abs + w.abs == 1 && u.abs <= 1 && v.abs <= 1 && w.abs <= 1)
  }

  case class Position(x: Int, y: Int, z: Int) {
    override def toString = Seq(x, y, z).mkString("(", ",", ")")
  }

}
