

/**
  * Created by Gonzalo on 7/29/2016.
  */
object ChessKeyPad {


  type Move = Point


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




  def main(args: Array[String]): Unit = {
    //    var tree9 = new PointsNode(Point(2, -2))
    //    tree9.populate(keyPad, knight, 7)
    //    println("starting from 9=" + tree9.countLeaves)
    //    println(tree9.debugLeftBranch())
 }
}
