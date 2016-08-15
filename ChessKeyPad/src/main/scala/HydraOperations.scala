

/**
  * Operations for a hydra on a board, that don't belong to either of those class in terms of compilation unit, and for which I want to
  * retain the ability of independently testing.
  */
trait HydraOperations {

/**
    * Finds out the derivated Hydras of a given one, for the given piece in the context of the given board
    * @param hydra   the source hydra
    * @param board   the board in which this occurs
    * @param piece   the piece that provides potential movement abilities
    * @param withRepetition  whetner or not the resulting paths/hydras can contain repeated points
    * @return   the derivate hydras
    */
  def processHydra(hydra: Hydra, board: Board, piece: Piece, withRepetition: Boolean): List[Hydra] ={
    var resHydras: List[Hydra] = Nil;
    for (point <- hydra.heads){
      var destinations = piece.getSources(board, point);
      if (!withRepetition) {
        destinations = destinations diff hydra.heads
        destinations = destinations diff hydra.tail
      }
      if (destinations != Nil)
        resHydras = resHydras ::: List(Hydra(destinations, point :: hydra.tail))
    }
    resHydras
  }


  /**
    * repeats the "processing" of an hydra several times, first on the hydra itself, then in its
    * derivates, and so on.
    * @param hydra      the origin hydra
    * @param board      the board in whose context this occurs
    * @param piece      the piece that provides movement abilities
    * @param withRepetition  whether or not paths/hydras can contain the same point more than once
    * @param times      how many levels we want to calculate
    * @return           the last (not the accumulation) result of processing resulting hydras
    */
  def repeatedlyProcessHydra(hydra: Hydra, board: Board, piece: Piece, withRepetition: Boolean, times: Int): List[Hydra] ={
    var resHydras: List[Hydra] = List(hydra);
    for (time <- 1 to times) {
      var partialList: List[Hydra] = Nil;
      for (hy <- resHydras) {
        partialList = partialList ::: processHydra(hy, board, piece, withRepetition);
      }
      resHydras = partialList
    }
    resHydras
  }
}
