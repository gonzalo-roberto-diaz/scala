import scala.collection.mutable.ListBuffer

/**
  * This trait includes some vector and point operations that don't really belong to any specific class in terms of
  * "compilation unit", and which I want to test independently.
  */
trait  PointOperations {

  type Move = Point


  def applyVectorToPoint(board: Board, origin: Point, vector: Move, repeatable: Boolean): List[Point] = {

    def recursiveApplication(board: Board, origin: Point, vector: Move): List[Point] = {
      val endPosition = origin + vector
      if (!board.issInside(endPosition)) Nil
      else endPosition :: recursiveApplication(board, endPosition, vector)
    }

    if (!repeatable) {
      val endPosition = origin + vector
      if (!board.issInside(endPosition)) Nil
      else List(endPosition)
    } else {
      recursiveApplication(board, origin, vector)
    }
  }

  def moves(board: Board, origin: Point, movAbility: MovAbility): List[Point] = {

    var vectors = List(movAbility.vector)
    if (movAbility.coordFlip) vectors = vectors ::: vectors.map(vec => vec.flipCoords)
    if (movAbility.mirrorX) vectors = vectors ::: vectors.map(vec => vec.mirrorX)
    if (movAbility.mirrorY) vectors = vectors ::: vectors.map(vec => vec.mirrorY)
    if (movAbility.centralSym) vectors = vectors ::: vectors.map(vec => -vec)

    var res: List[Point] = List()
    for (vec <- vectors) {
      res = res ::: applyVectorToPoint(board, origin, vec, movAbility.repeatable)
    }
    res
  }

  def enrichGivenBoardPieceAndPoint(board: Board, piece: Piece)(point: Point): List[Point] ={
    val derivates: ListBuffer[Point] = ListBuffer()
    piece.movAbilities.foreach(ma => {
      val points = moves(board, point, ma)
      derivates.appendAll(points)
    })
    derivates.toList
  }



}
