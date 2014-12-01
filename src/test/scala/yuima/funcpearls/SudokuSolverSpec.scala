package yuima.funcpearls

import org.scalatest.{Matchers, WordSpec}

/**
 * @author Yuichiroh Matsubayashi
 *         Created on 14/11/30.
 */
class SudokuSolverSpec extends WordSpec with Matchers {
  "SudokuSolver" when {
    "the board size is 4 * 4 and box size is 2 * 2" should {
      val solver = new SudokuSolver(4, 2, "1234".toList)

      val board = List(
        List('1', '.', '3', '.'),
        List('3', '.', '.', '2'),
        List('.', '.', '4', '.'),
        List('4', '.', '2', '3')
      )

      """create a list of columns.""" in {
        solver.cols(board) should be(List(
          List('1', '3', '.', '4'),
          List('.', '.', '.', '.'),
          List('3', '.', '4', '2'),
          List('.', '2', '.', '3')
        ))
      }

      """create a list of boxes.""" in {
        solver.boxes(board) should be(List(
          List('1', '.', '3', '.'),
          List('3', '.', '.', '2'),
          List('.', '.', '4', '.'),
          List('4', '.', '2', '3')
        ))
      }

      """ satisfies a condition that boxes(boxes(board)) == board.""" in {
        solver.boxes(solver.boxes(board)) should be(board)
      }

      """generate candidates for empty cells.""" in {
        solver.choices(board) should be(
          List(
            List(List('1'), List('1', '2', '3', '4'), List('3'), List('1', '2', '3', '4')),
            List(List('3'), List('1', '2', '3', '4'), List('1', '2', '3', '4'), List('2')),
            List(List('1', '2', '3', '4'), List('1', '2', '3', '4'), List('4'), List('1', '2', '3', '4')),
            List(List('4'), List('1', '2', '3', '4'), List('2'), List('3'))
          ))
      }

      """prune candidates that are already used in other cells in the same row.""" in {
        solver.pruneBy[Char](solver.rows)(solver.choices(board)) should be(
          List(
            List(List('1'), List('2', '4'), List('3'), List('2', '4')),
            List(List('3'), List('1', '4'), List('1', '4'), List('2')),
            List(List('1', '2', '3'), List('1', '2', '3'), List('4'), List('1', '2', '3')),
            List(List('4'), List('1'), List('2'), List('3'))
          ))
      }

      """prune candidates that are already used in other cells in the same col.""" in {
        solver.pruneBy[Char](solver.cols)(solver.choices(board)) should be(
          List(
            List(List('1'), List('1', '2', '3', '4'), List('3'), List('1', '4')),
            List(List('3'), List('1', '2', '3', '4'), List('1'), List('2')),
            List(List('2'), List('1', '2', '3', '4'), List('4'), List('1', '4')),
            List(List('4'), List('1', '2', '3', '4'), List('2'), List('3'))
          ))
      }

      """prune candidates that are already used in other cells in the same box.""" in {
        solver.pruneBy[Char](solver.boxes)(solver.choices(board)) should be(
          List(
            List(List('1'), List('2', '4'), List('3'), List('1', '4')),
            List(List('3'), List('2', '4'), List('1', '4'), List('2')),
            List(List('1', '2', '3'), List('1', '2', '3'), List('4'), List('1')),
            List(List('4'), List('1', '2', '3'), List('2'), List('3'))
          ))
      }

      """prune candidates that are already used in other cells in the same row, col and box.""" in {
        solver.prune(solver.choices(board)) should be(
          List(
            List(List('1'), List('2'), List('3'), List('4')),
            List(List('3'), List('4'), List('1'), List('2')),
            List(List('2'), List('2', '3'), List('4'), List('1')),
            List(List('4'), List('1'), List('2'), List('3'))
          ))
      }

      """solve the problem.""" in {
        solver.solutions(board) should be(
          List(List(
            List('1', '2', '3', '4'),
            List('3', '4', '1', '2'),
            List('2', '3', '4', '1'),
            List('4', '1', '2', '3')
          )))
      }

      """print a board.""" in {
        solver.showBoard(board) should be(
          """1 . | 3 .
            |3 . | . 2
            |----+----
            |. . | 4 .
            |4 . | 2 3""".stripMargin
        )
      }
    }

    """a certain 9 * 9 board:
      |. . . | 5 . . | . . 4
      |. 7 . | 6 . 9 | 2 3 .
      |6 . . | . . . | . 9 .
      |------+-------+------
      |5 . . | 2 . . | . . .
      |. . . | . . 6 | . 2 .
      |. . 2 | 1 7 . | . 4 6
      |------+-------+------
      |2 9 . | . 6 . | 4 . .
      |3 . 5 | . 8 . | 6 . .
      |. . . | . . 7 | . . 2
      | is given""".stripMargin should {

      val solver = new SudokuSolver(9, 3)

      val board = List(
        List('.', '.', '.', '5', '.', '.', '.', '.', '4'),
        List('.', '7', '.', '6', '.', '9', '2', '3', '.'),
        List('6', '.', '.', '.', '.', '.', '.', '9', '.'),
        List('5', '.', '.', '2', '.', '.', '.', '.', '.'),
        List('.', '.', '.', '.', '.', '6', '.', '2', '.'),
        List('.', '.', '2', '1', '7', '.', '.', '4', '6'),
        List('2', '9', '.', '.', '6', '.', '4', '.', '.'),
        List('3', '.', '5', '.', '8', '.', '6', '.', '.'),
        List('.', '.', '.', '.', '.', '7', '.', '.', '2')
      )

      println(solver.showBoard(board))

      """solve the problem.""" in {
        println
        solver.showBoard(solver.solutions(board)(0)) should be(
          """9 2 3 | 5 1 8 | 7 6 4
            |1 7 8 | 6 4 9 | 2 3 5
            |6 5 4 | 7 2 3 | 8 9 1
            |------+-------+------
            |5 6 9 | 2 3 4 | 1 8 7
            |7 4 1 | 8 9 6 | 5 2 3
            |8 3 2 | 1 7 5 | 9 4 6
            |------+-------+------
            |2 9 7 | 3 6 1 | 4 5 8
            |3 1 5 | 4 8 2 | 6 7 9
            |4 8 6 | 9 5 7 | 3 1 2""".stripMargin)
      }
    }
  }
}
