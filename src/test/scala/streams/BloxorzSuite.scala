package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("block is standing") {
    new Level1 {
      val pos = Pos(1,3)
      val block = Block(pos, pos)
      assert(block.isStanding)
   }
  }

  test("block is not standing") {
    new Level1 {
      val block = Block(Pos(1,1), Pos(1,2))
      assert(!block.isStanding)
    }
  }

  test("block is inside the terrain") {
    new Level1 {
      val block = Block(Pos(0,0), Pos(0,1))
      assert(block.isLegal)
    }
  }

  test("block is not inside the terrain") {
    new Level1 {
      val block = Block(Pos(3,0), Pos(3,1))
      assert(!block.isLegal)
    }
  }

  test("checking startBlock") {
    new Level1 {
      assert(startBlock.b1 == startPos)
      assert(startBlock.b2 == startPos)
    }
  }

  test("checking neighbors") {
    new Level1 {
      val neighbors = startBlock.neighbors
      val left = neighbors(0)._1
      val right = neighbors(1)._1
      val up = neighbors(2)._1
      val down = neighbors(3)._1
      assert(left == Block(Pos(1,-1), Pos(1,0)))
      assert(right == Block(Pos(1,2), Pos(1,3)))
      assert(up == Block(Pos(-1,1), Pos(0,1)))
      assert(down == Block(Pos(2,1), Pos(3,1)))
    }
  }

  test("checking legal neighbors") {
    new Level1 {
      val neighbors = startBlock.legalNeighbors
      val right = neighbors(0)._1
      val down = neighbors(1)._1
      assert(neighbors.length == 2)
      assert(right == Block(Pos(1,2), Pos(1,3)))
      assert(down == Block(Pos(2,1), Pos(3,1)))
    }
  }

  test("block is at the final position") {
    new Level1 {
      assert(done(Block(goal, goal)))
    }
  }

  test("block is not at the final position") {
    new Level1 {
      assert(!done(Block(startPos, startPos)))
    }
  }

  /**
   * neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)) returns
   * a stream with the following elements
   * (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up))
   * (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
   */
  test("neighborsWithHistory") {
    new Level1 {
      val next = neighborsWithHistory(startBlock, List(Left,Up))
      val neighbors = startBlock.legalNeighbors
      val right = neighbors(0)._1
      val down = neighbors(1)._1
      val first = next(0)
      val second = next(1)
      assert(first._1 == right)
      assert(first._2 == List(Right,Left,Up))
      assert(second._1 == down)
      assert(second._2 == List(Down,Left,Up))
      assert(next.length == 2)
    }
  }

  /**
   * newNeighborsOnly(
   *   Set(
   *     (Block(Pos(1,2),Pos(1,3)),List(Right, Left, Up)),
   *     (Block(Pos(2,1),Pos(3,1)),List(Down, Left, Up))
   *   ).toStream,
   *   Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
   * ))
   * returns
   * Stream((Block(Pos(2,1),Pos(3,1)),List(Down, Left, Up)))
   */
  test("newNeighborsOnly") {
    new Level1 {
      val p11 = Pos(1,1)
      val p12 = Pos(1,2)
      val p13 = Pos(1,3)
      val p21 = Pos(2,1)
      val p31 = Pos(3,1)
      val b1213 = Block(p12,p13)
      val b2131 = Block(p21,p31)
      val b1111 = Block(p11,p11)
      val b1 = (b1213, List(Right,Left,Up))
      val b2 = (b2131, List(Down,Left,Up))
      val neighbors = Set(b1,b2).toStream
      val explored = Set(b1213, b1111)
      val newNeighbors = newNeighborsOnly(neighbors, explored)
      assert(newNeighbors(0)._1 == b2131)
      assert(newNeighbors.length == 1)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
