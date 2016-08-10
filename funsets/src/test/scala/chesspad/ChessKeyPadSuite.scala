package chesspad

import chesspad.ChessKeyPad.{Movement, Point}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class ChessKeyPadSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }

  test("apply a movement to a point"){
    val knightMove = Movement(Point(1,2))
    val origin = Point(0,0)
    val res = knightMove(origin)

    assert(res.x == 1)
    assert(res.y == 2)

  }


  test("simple knight move validity") {
    val knightMove = Movement(Point(1,2))
    assert(knightMove.valid(Point(0,0)))
    assert(knightMove.valid(Point(1,0)))
    assert(!knightMove.valid(Point(2,0)))
  }


//  trait TestSets {
//    val s1 = singletonSet(1)
//    val s2 = singletonSet(2)
//    val s3 = singletonSet(3)
//  }
//
//
//
//  test("union contains all elements of each set") {
//    new TestSets {
//      val s = union(s1, s2)
//      assert(contains(s, 1), "Union 1")
//      assert(contains(s, 2), "Union 2")
//      assert(!contains(s, 3), "Union 3")
//    }
//  }

}
