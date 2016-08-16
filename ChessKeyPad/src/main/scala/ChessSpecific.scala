

/**
  * Movement abilities and pieces that are specific to the game of chess.
  */
trait ChessSpecific {

  // atomic moves
  val rookMovAb = MovAbility(Point(0, 1), repeatable = true, coordFlip = true, centralSym = true)
  val bishopMovAb = MovAbility(Point(1, 1), repeatable = true, mirrorX = true, mirrorY = true)
  val unaryRookMovAb = MovAbility(Point(0, 1), coordFlip = true, centralSym = true)
  val unaryBishopMovAb = MovAbility(Point(1, 1), mirrorX = true, mirrorY = true)
  val knightMovAb = MovAbility(Point(1, 2), coordFlip =  true, mirrorX =  true, centralSym =  true)
  val normalPeonMovAb = MovAbility(Point(0, -1))
  val eatingPeonMovAb = MovAbility(Point(1, -1), mirrorY = true)

  // Chess pieces
  val peon = Piece("peon", List(normalPeonMovAb, eatingPeonMovAb))
  val knight = Piece("knight", List(knightMovAb))
  val bishop = Piece("bishop", List(bishopMovAb))
  val rook = Piece("rook", List(rookMovAb))
  val queen = Piece("queen", List(bishopMovAb, rookMovAb))
  val king = Piece("king", List(unaryBishopMovAb, unaryRookMovAb))

}
