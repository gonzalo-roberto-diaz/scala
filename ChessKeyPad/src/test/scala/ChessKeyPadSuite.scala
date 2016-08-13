import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ChessKeyPad._

@RunWith(classOf[JUnitRunner])
class ChessKeyPadSuite extends FunSuite with VectorToPoint with ChessSpecific{

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


  test("values from a kepPad") {
    val keyPad = createKeyPad()
    assert(keyPad.valueOf(0, -2) == "7")
  }

  def createCleanSquareBoard(side: Int): Board = {
    val vec = for (y <- 0 until side) yield List.tabulate(side)(x => f"$x $y")
    Board(vec.toList, List())
  }

  test("points from a board") {
    val twoXtwo = createCleanSquareBoard(2)
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
    val result = applyVectorToPoint(lShapedBoard, Point(0, 0), Point(0, -1), repeatable = false)
    assert(result == List(Point(0, -1)))
  }

  def create7x7Board: List[List[String]] = {
    val vec = for (y <- 0 to 7) yield List.tabulate(7)(x => f"$x $y")
    vec.toList
  }

  test("knignt moves in clean 7x7 board") {
    val sevenBoard = Board(create7x7Board, List())
    val resMoves = moves(sevenBoard, Point(2, -2), knightMovAb)
    assert(resMoves.size == 8)
    assert(resMoves.contains(Point(3, 0)))
    assert(resMoves.contains(Point(4, -1)))
    assert(resMoves.contains(Point(3, -4)))
    assert(resMoves.contains(Point(0, -1)))
  }

  test("knignt moves from 0 in a keypad") {
    val keyPad = createKeyPad()
    val pointZero = Point(1, -3)
    val resMoves = moves(keyPad, pointZero, knightMovAb)
    val pointFour = Point(0, -1)
    val pointSix = Point(2, -1)
    assert(resMoves.size == 2)
    assert(resMoves.contains(pointFour))
    assert(resMoves.contains(pointSix))
  }

  test("diagonal king moves in a clean 7x7 board") {
    val sevenBoard = Board(create7x7Board, List())
    val resMoves = moves(sevenBoard, Point(2, -2), unaryBishopMovAb)
    assert(resMoves.size == 4)
    assert(resMoves.contains(Point(3, -1)))
    assert(resMoves.contains(Point(3, -3)))
    assert(resMoves.contains(Point(1, -1)))
    assert(resMoves.contains(Point(1, -3)))
  }

  test("perpendicular king moves in a clean 7x7 board") {
    val sevenBoard = createCleanSquareBoard(7)
    val resMoves = moves(sevenBoard, Point(2, -2), unaryRookMovAb)
    assert(resMoves.size == 4)
    assert(resMoves.contains(Point(3, -2)))
    assert(resMoves.contains(Point(2, -1)))
    assert(resMoves.contains(Point(1, -2)))
    assert(resMoves.contains(Point(2, -3)))
  }

  test("bishop moves in a clean 3x3 board") {
    val board = createCleanSquareBoard(3)
    val resMoves = moves(board, Point(0, 0), bishopMovAb)
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

    val board = createCleanSquareBoard(3);
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

    val board = createCleanSquareBoard(3);
    val bishop = createBishop();
    val hydra = Hydra(List(a, c), List(e, g))

    val res = processHydra(hydra, board, bishop, true)

    assert(res == List(Hydra(List(e, i),List(a, e, g)), Hydra(List(e, g), List(c, e, g))))
  }

  private def createKeyPad(): Board = {
    Board(List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"), List("*", "0", "#")), List("*", "#"))
  }


  //keypad points
  val p1 = Point(0, 0)
  val p2 = Point(1, 0)
  val p3 = Point(2, 0)
  val p4 = Point(0, -1)
  val p5 = Point(1, -1)
  val p6 = Point(2, -1)
  val p7 = Point(0, -2)
  val p8 = Point(1, -2)
  val p9 = Point(2, -2)
  val p0 = Point(1, -3)


  test("hydra to string for keypad") {
    val keyPad = createKeyPad()
    val hydra = Hydra(List(p0, p1), List(p2, p3))
    val sh = hydra.toString(keyPad)
    assert(sh == "(0, 1)(2, 3)")
  }


  test("paths length 3 for a knight in a keypad, without repetitions, starting at 0"){
    val keyPad = createKeyPad()
    val knight = createKnight()
    val pointZero = Point(1, -3)
    val pathsLength3 = repeatedlyProcessHydra(Hydra(List(p0), Nil), keyPad, knight, false, 3)

    assert(pathsLength3.size == 4)
    assert(pathsLength3.contains(Hydra(List(p8), List(p3, p4, p0))))
    assert(pathsLength3.contains(Hydra(List(p2), List(p9, p4, p0))))
    assert(pathsLength3.contains(Hydra(List(p8), List(p1, p6, p0))))
    assert(pathsLength3.contains(Hydra(List(p2), List(p7, p6, p0))))
  }

  test("all 7-length paths for a knight in a keypad, with repetitions, removing those starting with 0 and 1"){
    val keyPad = createKeyPad()

    var pieceHydras: List[Hydra] = Nil
    for (endPoint <- keyPad.getPoints){
      val endHydra = Hydra(List(endPoint), Nil)
      pieceHydras = pieceHydras ::: repeatedlyProcessHydra(endHydra, keyPad, knight, true, 6);
    }

    pieceHydras = pieceHydras.map(kh => Hydra(kh.heads diff List(p0, p1), kh.tail)).filter(h => !h.heads.isEmpty)

    // the number of paths is known to be 952
    assert(pieceHydras.map(kh => kh.heads.size).sum == 952)

    //pieceHydras.foreach(h => println(h.toString(keyPad)))
  }

  test("all 7-length paths for a bishop in a keypad, with repetitions, removing those starting with 0 and 1"){
    val keyPad = createKeyPad()

    var pieceHydras: List[Hydra] = Nil
    for (endPoint <- keyPad.getPoints){
      val endHydra = Hydra(List(endPoint), Nil)
      pieceHydras = pieceHydras ::: repeatedlyProcessHydra(endHydra, keyPad, bishop, true, 6);
    }

    pieceHydras = pieceHydras.map(kh => Hydra(kh.heads diff List(p0, p1), kh.tail)).filter(h => !h.heads.isEmpty)

    // the number of paths is known to be 2431
    assert(pieceHydras.map(kh => kh.heads.size).sum == 2341)
  }


  test("all 7-length paths for a rook in a keypad, with repetitions, removing those starting with 0 and 1"){
    val keyPad = createKeyPad()

    var pieceHydras: List[Hydra] = Nil
    for (endPoint <- keyPad.getPoints){
      val endHydra = Hydra(List(endPoint), Nil)
      pieceHydras = pieceHydras ::: repeatedlyProcessHydra(endHydra, keyPad, rook, true, 6);
    }

    pieceHydras = pieceHydras.map(kh => Hydra(kh.heads diff List(p0, p1), kh.tail)).filter(h => !h.heads.isEmpty)

    // the number of paths is known to be 49362
    assert(pieceHydras.map(kh => kh.heads.size).sum == 49326)
  }

  test("all 7-length paths for a queen in a keypad, with repetitions, removing those starting with 0 and 1"){
    val keyPad = createKeyPad()

    var pieceHydras: List[Hydra] = Nil
    for (endPoint <- keyPad.getPoints){
      val endHydra = Hydra(List(endPoint), Nil)
      pieceHydras = pieceHydras ::: repeatedlyProcessHydra(endHydra, keyPad, queen, true, 6);
    }

    pieceHydras = pieceHydras.map(kh => Hydra(kh.heads diff List(p0, p1), kh.tail)).filter(h => !h.heads.isEmpty)

    // the number of paths is known to be 751503
    assert(pieceHydras.map(kh => kh.heads.size).sum == 751503)
  }

  test("all 7-length paths for a peon in a keypad, with repetitions, removing those starting with 0 and 1"){
    val keyPad = createKeyPad()

    var pieceHydras: List[Hydra] = Nil
    for (endPoint <- keyPad.getPoints){
      val endHydra = Hydra(List(endPoint), Nil)
      pieceHydras = pieceHydras ::: repeatedlyProcessHydra(endHydra, keyPad, peon, true, 6);
    }

    pieceHydras = pieceHydras.map(kh => Hydra(kh.heads diff List(p0, p1), kh.tail)).filter(h => !h.heads.isEmpty)

    // the number of paths is known to be 0
    assert(pieceHydras.map(kh => kh.heads.size).sum == 0)
  }

  test("all 7-length paths for a king in a keypad, with repetitions, removing those starting with 0 and 1"){
    val keyPad = createKeyPad()

    var pieceHydras: List[Hydra] = Nil
    for (endPoint <- keyPad.getPoints){
      val endHydra = Hydra(List(endPoint), Nil)
      pieceHydras = pieceHydras ::: repeatedlyProcessHydra(endHydra, keyPad, king, true, 6);
    }

    pieceHydras = pieceHydras.map(kh => Hydra(kh.heads diff List(p0, p1), kh.tail)).filter(h => !h.heads.isEmpty)

    // the number of paths is known to be 124908
    assert(pieceHydras.map(kh => kh.heads.size).sum == 124908)
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