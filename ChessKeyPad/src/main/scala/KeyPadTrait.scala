import scala.collection.mutable.Map

/**
  * resources that are specific not only to the game of chess, but to the quiz
  */
trait KeyPadTrait extends ChessSpecific{

  //keypad Points
  val kp1 = Point(0, 0)
  val kp2 = Point(1, 0)
  val kp3 = Point(2, 0)
  val kp4 = Point(0, -1)
  val kp5 = Point(1, -1)
  val kp6 = Point(2, -1)
  val kp7 = Point(0, -2)
  val kp8 = Point(1, -2)
  val kp9 = Point(2, -2)
  val kp0 = Point(1, -3)

  val keyPad = Board(List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"), List("*", "0", "#")), List("*", "#"))

  //functions
  val pointStringifier: Point => String = (point: Point) => {
    val sb = new StringBuilder
    sb += '('
    sb ++= point.x.toString
    sb += ','
    sb ++= point.y.toString
    sb += ')'
    sb.toString
  }

  val pointStringifierByKeypad: Point => String = (point: Point) => {
    keyPad.valueOf(point)
  }

  def pointDerivatorByBoardPieceAndPoint(board: Board, piece: Piece) (origin: Point): List[Point] = {
    piece.getSources(board, origin)
  }

  def uncachedKnightDerivator = pointDerivatorByBoardPieceAndPoint(keyPad, knight)(_)
  def uncachedBishopDerivator = pointDerivatorByBoardPieceAndPoint(keyPad, bishop)(_)
  def uncachedRookDerivator = pointDerivatorByBoardPieceAndPoint(keyPad, rook)(_)
  def uncachedPeonDerivator = pointDerivatorByBoardPieceAndPoint(keyPad, peon)(_)
  def uncachedKingDerivator = pointDerivatorByBoardPieceAndPoint(keyPad, king)(_)
  def uncachedQueenDerivator = pointDerivatorByBoardPieceAndPoint(keyPad, queen)(_)

  //cached versions of the derivated functions
  //(they are nor really necessary), but slightly speed up the tests
  val knightCache: Map[Point, List[Point]] = Map()
  val bishopCache: Map[Point, List[Point]] = Map()
  val rookCache: Map[Point, List[Point]] = Map()
  val peonCache: Map[Point, List[Point]] = Map()
  val kingCache: Map[Point, List[Point]] = Map()
  val queenCache: Map[Point, List[Point]] = Map()


  def cachedPointDerivatorByBoardPieceAndPoint(cache: Map[Point, List[Point]], board: Board, piece: Piece) (origin: Point): List[Point] = {
    if (cache.contains(origin)) cache.get(origin).get
    else{
      val sources = piece.getSources(board, origin)
      cache.put(origin, sources)
      sources
    }
  }

  def knightDerivator = cachedPointDerivatorByBoardPieceAndPoint(knightCache, keyPad, knight)(_)
  def bishopDerivator = cachedPointDerivatorByBoardPieceAndPoint(bishopCache, keyPad, bishop)(_)
  def rookDerivator = cachedPointDerivatorByBoardPieceAndPoint(rookCache, keyPad, rook)(_)
  def peonDerivator = cachedPointDerivatorByBoardPieceAndPoint(peonCache, keyPad, peon)(_)
  def kingDerivator = cachedPointDerivatorByBoardPieceAndPoint(kingCache, keyPad, king)(_)
  def queenDerivator = cachedPointDerivatorByBoardPieceAndPoint(queenCache, keyPad, queen)(_)




}
