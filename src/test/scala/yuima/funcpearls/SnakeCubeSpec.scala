package yuima.funcpearls

import org.scalatest.{Matchers, WordSpec}

/**
 * @author Yuichiroh Matsubayashi
 *         Created on 14/11/28.
 */
class SnakeCubeSpec extends WordSpec with Matchers{
  "SnakeCube" when {
    "the size is 3*3*3" should {
      import SnakeCube._
      """have 27 small cubes""" in {
        snake.sum - (snake.size - 1) should be(27)
      }

      """create correct sections from the inputs (start position, direction, length)""" in {
        section(Position(1, 1, 1))(Direction(0, 1, 0))(3) should be(Stream(Position(1,2,1), Position(1,1,1)))
        section(Position(1, 3, 1))(Direction(1, 0, 0))(3) should be(Stream(Position(2,3,1), Position(1,3,1)))
      }
    }
  }
}
