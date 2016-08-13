import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ChessKeyPad._

@RunWith(classOf[JUnitRunner])
class ChessKeyPadSuite extends FunSuite {

  test("central symmetry of a point, always using (0,0) as origin"){
    val p = Point(1, 2)
    val cSym = -p
    assert(cSym == Point(-1, -2))
  }




  val lShapedBoard = Board(List(List("0 0"), List("0 -1"), List("0 -2", "1 -2")), List())

  test("weird l-shaped baord isInside") {
    //out of bounds
    assert(!lShapedBoard.issInside(Point(2, -1)))
    assert(!lShapedBoard.issInside(Point(0, 5)))
    //zeroes
    assert(!lShapedBoard.issInside(Point(1, 0)))
    assert(!lShapedBoard.issInside(Point(1, 1)))
    //inside
    assert(lShapedBoard.issInside(Point(0, 0)))
    assert(lShapedBoard.issInside(Point(0, -1)))
    assert(lShapedBoard.issInside(Point(0, -2)))
    assert(lShapedBoard.issInside(Point(1, -2)))
  }

  test("values") {
    assert(lShapedBoard.valueOf(0, 0) == "0 0")
    assert(lShapedBoard.valueOf(0, -1) == "0 -1")
  }

  test("points from a board") {
    val twoXtwo = Board(createCleanSquareBoard(2), Nil);
    assert(twoXtwo.getPoints == List(Point(0, 0), Point(1, 0), Point(0, -1), Point(1, -1)))
  }

  test("points from a board with forbidden values") {
    val keyPad = Board( List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"), List("*", "0", "#")), List("*", "#"))
    var points = List(Point(0, 0), Point(1, 0), Point(2, 0))
    points = points ::: List(Point(0, -1), Point(1, -1), Point(2, -1))
    points = points ::: List(Point(0, -2), Point(1, -2), Point(2, -2))
    points = points ::: List(Point(1, -3))
    assert(keyPad.getPoints == points)
  }


  test("apply vector not repeatable") {
    val result = applyVectorToPoint(lShapedBoard, Point(0, 0), Point(0, -1), false)
    assert(result == List(Point(0, -1)))
  }

  def create7x7Board: List[List[String]] = {
    val vec = for (y <- 0 to 7) yield List.tabulate(7)(x => f"$x $y")
    vec.toList
  }

  test("knignt moves in clean 7x7 board") {
    val sevenBoard = Board(create7x7Board, List())
    val knightMov = MovAbility(Point(1, 2), false, true, true, false, true)
    val resMoves = moves(sevenBoard, Point(2, -2), knightMov)
    assert(resMoves.size == 8)
    assert(resMoves.contains(Point(3, 0)))
    assert(resMoves.contains(Point(4, -1)))
    assert(resMoves.contains(Point(3, -4)))
    assert(resMoves.contains(Point(0, -1)))
  }


  test("diagonal king moves in a clean 7x7 board") {
    val sevenBoard = Board(create7x7Board, List())
    val kingMovDiag = MovAbility(Point(1, 1), false, false, true, false, true)
    val resMoves = moves(sevenBoard, Point(2, -2), kingMovDiag)
    assert(resMoves.size == 4)
    assert(resMoves.contains(Point(3, -1)))
    assert(resMoves.contains(Point(3, -3)))
    assert(resMoves.contains(Point(1, -1)))
    assert(resMoves.contains(Point(1, -3)))
  }

  test("perpendicular king moves in a clean 7x7 board") {
    val sevenBoard = Board(createCleanSquareBoard(7), List())
    val kingMovPerp = MovAbility(Point(0, 1), false, true, false, false, true)
    val resMoves = moves(sevenBoard, Point(2, -2), kingMovPerp)
    assert(resMoves.size == 4)
    assert(resMoves.contains(Point(3, -2)))
    assert(resMoves.contains(Point(2, -1)))
    assert(resMoves.contains(Point(1, -2)))
    assert(resMoves.contains(Point(2, -3)))
  }

  test("bishop moves in a clean 3x3 board") {
    val board = Board(createCleanSquareBoard(3), List())
    val bishopMoves = MovAbility(Point(1, -1), true, false, true, true, false)
    //val bishop = Piece("bishop", List(bishopMoves))
    val resMoves = moves(board, Point(0, 0), bishopMoves)
    assert(resMoves.size == 2)
    assert(resMoves.contains(Point(1, -1)))
    assert(resMoves.contains(Point(2, -2)))
  }

  test("apply points to a MultiHeadList with empty tail unary head, without repetition") {
    val a = Point(0, 0)
    val b = Point(1, 1)
    val c = Point(2, 2)

    val mhList = Hydra(List(a), Nil)
    val parentPoints = List(b, c)
    val result = mhList.applyPoints(parentPoints, false)
    val r1 = Hydra(List(b, c), List(a))
    assert(result == List(r1))
  }

  test("apply points to a MultiHeadList with non-unary tail, non-unary head, without repetition") {
    val a = Point(0, 0)
    val b = Point(1, 1)
    val c = Point(2, 2)
    val d = Point(3, 3)
    val e = Point(4, 4)
    val f = Point(5, 5)

    val mhList = Hydra(List(a, b), List(c, d))
    val parentPoints = List(e, f)
    val result = mhList.applyPoints(parentPoints, false)
    val r1 = Hydra(List(e, f), List(a, c, d))
    val r2 = Hydra(List(e, f), List(b, c, d))
    assert(result == List(r1, r2))
  }

  test("apply repeated points to a MultiHeadList with non-unary tail, non-unary head, without repetition") {
    val a = Point(0, 0)
    val b = Point(1, 1)
    val c = Point(2, 2)
    val d = Point(3, 3)
    val e = Point(4, 4)

    val mhList = Hydra(List(a, b), List(c, d))
    val parentPoints = List(a, c, e)
    val result = mhList.applyPoints(parentPoints, false)
    val r1 = Hydra(List(e), List(a, c, d))
    val r2 = Hydra(List(e), List(b, c, d))
    assert(result == List(r1, r2))
  }

  test("apply repeated points to a MultiHeadList with non-unary tail, non-unary head, with repetition") {
    val a = Point(0, 0)
    val b = Point(1, 1)
    val c = Point(2, 2)
    val d = Point(3, 3)
    val e = Point(4, 4)

    val mhList = Hydra(List(a, b), List(c, d))
    val parentPoints = List(a, c, e)
    val result = mhList.applyPoints(parentPoints, true)
    val r1 = Hydra(List(a, c, e), List(a, c, d))
    val r2 = Hydra(List(a, c, e), List(b, c, d))
    assert(result == List(r1, r2))
  }

  test("process a hydra without repetitions") {
    val a = Point(0, 0)
    val b = Point(1, 0)
    val c = Point(2, 0)
    val d = Point(0, -1)
    val e = Point(1, -1)
    val f = Point(2, -1)
    val g = Point(0, -2)
    val h = Point(1, -2)
    val i = Point(2, -2)

    val board = Board(createCleanSquareBoard(3), List());
    val bishop = createBishop();
    val hydra = Hydra(List(a, c), List(e, g))

    val res = processHydra(hydra, board, bishop, false)

    assert(res == List(Hydra(List(i),List(a, e, g))))
  }

  test("process a hydra with repetitions") {
    val a = Point(0, 0)
    val b = Point(1, 0)
    val c = Point(2, 0)
    val d = Point(0, -1)
    val e = Point(1, -1)
    val f = Point(2, -1)
    val g = Point(0, -2)
    val h = Point(1, -2)
    val i = Point(2, -2)

    val board = Board(createCleanSquareBoard(3), List());
    val bishop = createBishop();
    val hydra = Hydra(List(a, c), List(e, g))

    val res = processHydra(hydra, board, bishop, true)

    assert(res == List(Hydra(List(e, i),List(a, e, g)), Hydra(List(e, g), List(c, e, g))))
  }



  private def createKnight(): Piece ={
    val knightMov = MovAbility(Point(1, 2), false, true, true, false, true)
    Piece("knight", List(knightMov))
  }

  private def createBishop(): Piece ={
    val mov =   MovAbility(Point(1, -1), true, false, true, true, false)
    Piece("bishop", List(mov))
  }

  private def createRook(): Piece ={
    val mov =   MovAbility(Point(1, 0), true, true, false, false, true)
    Piece("rook", List(mov))
  }







}