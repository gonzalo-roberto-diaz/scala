
/**
  * A class that represent a piece in any board game, as long as its movement can be interpreted as vectors starting from (0,0)
  * @param name             the name of the piece
  * @param movAbilities     a list of all the "movement abilities" of that piece
  */
case class Piece(name: String, movAbilities: List[MovAbility]) extends PointOperations{


  /**
    * given a point, it returns those points that can be the source of the piece's movements, ending in it. Equivalent to "getDestinations()"
    * for most cases, except for those pieces with non-symmetrical movement abilities.
    * @param board    the board
    * @param origin   the startinf point
    * @return
    */
  def getSources(board: Board, origin: Point): List[Point] = {
    val reverseMovAbilities = movAbilities.map(ma => MovAbility(-ma.vector, ma.repeatable, ma.coordFlip, ma.mirrorX, ma.mirrorY, ma.centralSym))
    val twoDList = for (elem <- reverseMovAbilities) yield moves(board, origin, elem)
    twoDList.flatten
  }

  /**
    * given a point in a board, it returns all valid destination points
    * @param board      the board
    * @param origin     the starting point
    * @return           a list of possible destination points, sorted by their natural order
    */
  def getDestinations(board: Board, origin: Point): List[Point] = {
    val twoDList = for (elem <- this.movAbilities) yield moves(board, origin, elem)
    twoDList.flatten
  }

}
