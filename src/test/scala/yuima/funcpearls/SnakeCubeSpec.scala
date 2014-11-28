package yuima.funcpearls

import org.scalatest.{Matchers, WordSpec}
import yuima.funcpearls.SnakeCube._

/**
 * @author Yuichiroh Matsubayashi
 *         Created on 14/11/28.
 */
class SnakeCubeSpec extends WordSpec with Matchers{
  "SnakeCube" when {
    "the size is 3*3*3" should {
      """have 27 small cubes""" in {
        val puzzle = standard
        val snake = puzzle.sections
        snake.sum - (snake.size - 1) should be(27)
      }

      """create correct sections from the inputs (start position, direction, length)""" in {
        section(Position(1, 1, 1))(Direction(0, 1, 0))(3) should be(Stream(Position(1,2,1), Position(1,1,1)))
        section(Position(1, 3, 1))(Direction(1, 0, 0))(3) should be(Stream(Position(2,3,1), Position(1,3,1)))
      }

      """solve the standard problem correctly.""" in {
        solutions(standard).size should be(4)
        List(List(Position(3,3,3), Position(2,3,3)), List(Position(1,3,3), Position(1,2,3)), List(Position(1,1,3), Position(2,1,3)), List(Position(3,1,3), Position(3,1,2)), List(Position(3,1,1)), List(Position(2,1,1)), List(Position(2,1,2)), List(Position(1,1,2), Position(1,2,2)), List(Position(1,3,2), Position(2,3,2)), List(Position(3,3,2)), List(Position(3,3,1)), List(Position(3,2,1), Position(3,2,2)), List(Position(3,2,3)), List(Position(2,2,3), Position(2,2,2)), List(Position(2,2,1)), List(Position(2,3,1)), List(Position(1,3,1), Position(1,2,1)), List(Position(1,1,1)))
        List(List(Position(3,3,3), Position(3,3,2)), List(Position(3,3,1), Position(3,2,1)), List(Position(3,1,1), Position(3,1,2)), List(Position(3,1,3), Position(2,1,3)), List(Position(1,1,3)), List(Position(1,1,2)), List(Position(2,1,2)), List(Position(2,1,1), Position(2,2,1)), List(Position(2,3,1), Position(2,3,2)), List(Position(2,3,3)), List(Position(1,3,3)), List(Position(1,2,3), Position(2,2,3)), List(Position(3,2,3)), List(Position(3,2,2), Position(2,2,2)), List(Position(1,2,2)), List(Position(1,3,2)), List(Position(1,3,1), Position(1,2,1)), List(Position(1,1,1)))
        List(List(Position(3,3,3), Position(3,3,2)), List(Position(3,3,1), Position(2,3,1)), List(Position(1,3,1), Position(1,3,2)), List(Position(1,3,3), Position(1,2,3)), List(Position(1,1,3)), List(Position(1,1,2)), List(Position(1,2,2)), List(Position(1,2,1), Position(2,2,1)), List(Position(3,2,1), Position(3,2,2)), List(Position(3,2,3)), List(Position(3,1,3)), List(Position(2,1,3), Position(2,2,3)), List(Position(2,3,3)), List(Position(2,3,2), Position(2,2,2)), List(Position(2,1,2)), List(Position(3,1,2)), List(Position(3,1,1), Position(2,1,1)), List(Position(1,1,1)))
        List(List(Position(3,3,3), Position(3,2,3)), List(Position(3,1,3), Position(2,1,3)), List(Position(1,1,3), Position(1,2,3)), List(Position(1,3,3), Position(1,3,2)), List(Position(1,3,1)), List(Position(1,2,1)), List(Position(1,2,2)), List(Position(1,1,2), Position(2,1,2)), List(Position(3,1,2), Position(3,2,2)), List(Position(3,3,2)), List(Position(3,3,1)), List(Position(2,3,1), Position(2,3,2)), List(Position(2,3,3)), List(Position(2,2,3), Position(2,2,2)), List(Position(2,2,1)), List(Position(3,2,1)), List(Position(3,1,1), Position(2,1,1)), List(Position(1,1,1)))
      }
    }
  }
}
