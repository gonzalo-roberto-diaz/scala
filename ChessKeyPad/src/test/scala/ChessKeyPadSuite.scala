import ChessKeyPad.{PointNode, _}
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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

  test("create a parent PointNode from a PointNode with empty children lists") {
    val start = PointNode(Point(1, 2), Nil)
    val point = Point(3, 4)
    val result = start.createParent(point)
    assert(result.childrenLists == List(List(start)))
  }

  test("create a parent PointNode from a PointNode with one children list") {
    val dummy = PointNode(Point(0, 0), Nil)
    val start = PointNode(Point(1, 2), List(List(dummy)))
    val point = Point(3, 4)
    val result = start.createParent(point)
    assert(result.value == point)
    assert(result.childrenLists == List(List(start.withoutList(), dummy.withoutList())))
  }

  test("create a parent PointNode from a PointNode with multiple children lists") {
    val dummy0 = PointNode(Point(0, 0), Nil)
    val dummy1 = PointNode(Point(1, 1), List(List(dummy0)))
    val dummy2 = PointNode(Point(2, 2), Nil)
    val start = PointNode(Point(8, 8), List(List(dummy1, dummy2), List(dummy1, dummy2)))
    val point = Point(3, 4)
    val result = start.createParent(point)
    assert(result.value == point)
    assert(result.childrenLists == List(List(start.withoutList(), dummy1.withoutList(), dummy2.withoutList()), List(start.withoutList(), dummy1.withoutList(), dummy2.withoutList())))
  }

  test("node not containing a point anywhere, should allow it as parent for non-repeatables") {
    val dummy1 = PointNode(Point(0, 0), Nil)
    val dummy2 = PointNode(Point(0, 0), Nil)
    val start = PointNode(Point(1, 2), List(List(dummy1, dummy2), List(dummy1, dummy2)))
    val point = Point(3, 4)
    assert(start.allowedAsParentForNonRepeatables(point))
  }

  test("node containing a point as value, should not allow it as parent for non-repeatables") {
    val dummy1 = PointNode(Point(0, 0), Nil)
    val dummy2 = PointNode(Point(0, 0), Nil)
    val start = PointNode(Point(3, 4), List(List(dummy1, dummy2), List(dummy1, dummy2)))
    val point = Point(3, 4)
    assert(!start.allowedAsParentForNonRepeatables(point))
  }

  test("node containing a point as value of any list item, should not allow it as parent for non-repeatables") {
    val dummy1 = PointNode(Point(0, 0), Nil)
    val dummy2 = PointNode(Point(1, 2), Nil)
    val start = PointNode(Point(3, 4), List(List(dummy1, dummy2), List(dummy1, dummy2)))
    val point = Point(0, 0)
    assert(!start.allowedAsParentForNonRepeatables(point))
  }

  test("merging 2 nodes") {
    val dummy1 = PointNode(Point(1, 1), Nil)
    val dummy2 = PointNode(Point(2, 2), Nil)
    val dummy3 = PointNode(Point(3, 3), Nil)
    val dummy4 = PointNode(Point(4, 4), Nil)
    val one = PointNode(Point(0, 0), List(List(dummy1, dummy2)))
    val two = PointNode(Point(0, 0), List(List(dummy3, dummy4)))
    val res = one.merge(two)
    assert(res.value == Point(0, 0))
    assert(res.childrenLists == List(List(dummy1, dummy2), List(dummy3, dummy4)))
  }

  test("merging nodes in a value-orderer PointNode list containing some equal values") {
    val dummy1 = PointNode(Point(1, 1), Nil)
    val dummy2 = PointNode(Point(2, 2), Nil)
    val dummy3 = PointNode(Point(3, 3), Nil)
    val dummy4 = PointNode(Point(4, 4), Nil)
    val dummy5 = PointNode(Point(5, 5), Nil)
    val one = PointNode(Point(0, 0), List(List(dummy1, dummy2)))
    val two = PointNode(Point(0, 0), List(List(dummy3, dummy4)))
    val three = PointNode(Point(8, 8), List(List(dummy5)))
    val res =  mergeEqualValues(List(one, two, three))
    assert(res.size == 2)
    assert(res.head.value == Point(0, 0))
    assert(res.head.childrenLists == List(List(dummy1, dummy2), List(dummy3, dummy4)))
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

  test("non-repeating parents of 2 bishop destinations at the bottom corner of a 3x3 board") {
    val board= Board(createCleanSquareBoard(3), Nil)
    val ends = List(PointNode(Point(0, -2), Nil), PointNode(Point(2, -2), Nil))
    val bishop = createBishop()
    val parents = superLevel(board, bishop, ends, false)
    assert(parents.size == 3)
    parents.contains(PointNode(Point(1, -1), Nil))
    parents.contains(PointNode(Point(0, 0), Nil))
    parents.contains(PointNode(Point(0, 2), Nil))
  }

  test("non-repeating level-2 parents of a rook in a corner of a 2x2 board should return the opposite corner") {
    val board= Board(createCleanSquareBoard(2), Nil)
    val ends = List(PointNode(Point(0, -1), Nil))
    val rook = createRook()
    val parents = superLevelOrder(board, rook, ends, false, 2)
    assert(parents.size == 1)
    parents.contains(PointNode(Point(1, 0), Nil))
  }






}