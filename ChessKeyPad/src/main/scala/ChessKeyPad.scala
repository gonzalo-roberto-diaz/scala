import com.sun.javaws.exceptions.InvalidArgumentException
import com.sun.management.VMOption.Origin

import scala.collection.mutable

/**
  * Created by Gonzalo on 7/29/2016.
  */
object ChessKeyPad {


  type Move = Point


  def moves(board: Board, origin: Point, movAbility: MovAbility): List[Point] = {

    var vectors = List(movAbility.vector);
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


  def applyVectorToPoint(board: Board, origin: Point, vector: Move, repeatable: Boolean): List[Point] = {

    def recursiveApplication(board: Board, origin: Point, vector: Move): List[Point] = {
      val endPosition = origin + vector;
      if (!board.issInside(endPosition)) Nil
      else endPosition :: recursiveApplication(board, endPosition, vector)
    }

    if (!repeatable) {
      val endPosition = origin + vector;
      if (!board.issInside(endPosition)) Nil
      else List(endPosition)
    } else {
      recursiveApplication(board, origin, vector)
    }
  }


  case class MovAbility(vector: Point, repeatable: Boolean, coordFlip: Boolean, mirrorX: Boolean, mirrorY: Boolean, centralSym: Boolean);


  case class Piece(name: String, movAbilities: List[MovAbility]) {
    def getDestinations(board: Board, origin: Point): List[Point] = {
      val twoDList = for (elem <- this.movAbilities) yield moves(board, origin, elem)
      twoDList.flatten
    }
  }



  def processHydra(hydra: Hydra, board: Board, piece: Piece, withRepetition: Boolean): List[Hydra] ={
    var resHydras: List[Hydra] = Nil;
    for (point <- hydra.heads){
      var destinations = piece.getDestinations(board, point);
      if (!withRepetition) {
        destinations = destinations diff hydra.heads
        destinations = destinations diff hydra.tail
      }
      if (destinations != Nil)
        resHydras = resHydras ::: List(Hydra(destinations, point :: hydra.tail))
    }
    resHydras
  }

//  def superLevel(board: Board, piece: Piece, initial: List[PointNode], canRepeat: Boolean): List[PointNode] = {
//    var parents: List[PointNode] = Nil
//    for (node <- initial) {
//      //one move backwards
//      var destinations = piece.getDestinations(board, node.value)
//      if (!canRepeat) destinations = destinations.filter(point => node.allowedAsParentForNonRepeatables(point))
//      destinations.foreach(dest => {
//        val parentNode = node.createParent(dest)
//        parents = parents ::: List(parentNode)
//      })
//    }
//    parents = parents.sortBy(_.value)
//    parents = mergeEqualValues(parents)
//    parents
//  }
//
//  def superLevelOrder(board: Board, piece: Piece, initial: List[PointNode], canRepeat: Boolean, order: Int): List[PointNode] = {
//    var parents = initial;
//    for (level <- 0 to order - 1) {
//      parents = superLevel(board, piece, parents, canRepeat);
//    }
//    parents
//  }







  //  def populate(board: Board, piece: Piece, pathLength: Int): List[List[Point]] = {
  //
  //  }


  //  def movesOfLengthSize(board: Board, piece: Piece, origin: Point, length: Int): List[List[Point]] ={
  //
  //    def singleStep(origin: Point): List[Point] ={
  //      for (elem <- piece.movAbilities) yield moves(board, origin, elem).flatten
  //    }
  //
  //
  //
  //    val oneStep: List[Point] = singleStep(origin);
  //    if (length == 1) List(oneStep)
  //    else{
  //      val nextStep = for(elem <- oneStep) yield movesOfLengthSize(board, piece, elem, length-1)
  //    }
  //
  //
  //
  //  }


  val sevenBoard = Board(createCleanSquareBoard(7), List())


  def createCleanSquareBoard(side: Int): List[List[String]] = {
    val vec = for (y <- 0 to side - 1) yield List.tabulate(side)(x => f"$x $y")
    vec.toList
  }


  val bishopMoves = MovAbility(Point(1, -1), true, false, true, true, false)


  val bishop = Piece("bishop", List(bishopMoves))

  val threeBoard = Board(createCleanSquareBoard(3), List())

  val keyPad = Board(List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"), List("*", "0", "#")), List("*", "#"))

  val knightMov = MovAbility(Point(1, 2), false, true, true, false, true)
  val knight = Piece("knight", List(knightMov))

  def main(args: Array[String]): Unit = {
    //    var tree9 = new PointsNode(Point(2, -2))
    //    tree9.populate(keyPad, knight, 7)
    //    println("starting from 9=" + tree9.countLeaves)
    //    println(tree9.debugLeftBranch())
 }
}
