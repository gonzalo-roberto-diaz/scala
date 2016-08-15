case class Piece(name: String, movAbilities: List[MovAbility]) extends PointOperations{


  /**
    * given a point, it returns those points that can be the source of the piece's movements, ending in it. Equivalent to "getDestinations()"
    * in most cases, except for those pieces with non-symmetrical movement abilities.
    * @param board
    * @param origin
    * @return
    */
  def getSources(board: Board, origin: Point): List[Point] = {
    var reverseMovAbilities = movAbilities.map(ma => MovAbility(-ma.vector, ma.repeatable, ma.coordFlip, ma.mirrorX, ma.mirrorY, ma.centralSym))
    val twoDList = for (elem <- reverseMovAbilities) yield moves(board, origin, elem)
    twoDList.flatten
  }

  def getDestinations(board: Board, origin: Point): List[Point] = {
    val twoDList = for (elem <- this.movAbilities) yield moves(board, origin, elem)
    twoDList.flatten
  }

}

