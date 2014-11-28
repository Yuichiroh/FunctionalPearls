package yuima.funcpearls

/** Scala implementation of "Solving the Snake Cube Puzzle in Haskel."
  * http://web.cecs.pdx.edu/~mpj/snakecube/revised-SnakeCube.pdf
  * @author Yuichiroh Matsubayashi
  *         Created on 14/11/28.
  */
object SnakeCube extends App {
  /** define a snake */
  val snake = List(3, 2, 2, 3, 2, 3, 2, 2, 3, 3, 2, 2, 2, 3, 3, 3, 3)
  val cubes = snake.sum - (snake.size - 1)

  def inCube(size: Int)(pos: Position) = {
    def inRange(k: Int) = 1 <= k && k <= size

    inRange(pos.x) && inRange(pos.y) && inRange(pos.z)
  }

  def section(start: Position)(dir: Direction)(length: Int) = {
    def pieces = Stream.iterate(start) { pos: Position => Position(pos.x + dir.u, pos.y + dir.v, pos.z + dir.w)}
    pieces.take(length - 1).reverse
  }

  case class Direction(u: Int, v: Int, w: Int) {
    require(u.abs + v.abs + w.abs == 1 && u.abs <= 1 && v.abs <= 1 && w.abs <= 1)
  }

  case class Position(x: Int, y: Int, z: Int)

  case class Section(positions: List[Position])

  case class Solution(sections: List[Section])

}
