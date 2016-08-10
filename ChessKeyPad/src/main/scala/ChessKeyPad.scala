import com.sun.javaws.exceptions.InvalidArgumentException
import com.sun.management.VMOption.Origin

import scala.collection.mutable

/**
  * Created by Gonzalo on 7/29/2016.
  */
object ChessKeyPad {

  case class Point(x: Int, y: Int) extends Ordered[Point]{
    def +(move: Move): Point = Point(move.x + x, move.y + y)
    def mirrorX  : Point = Point(x, - y)
    def mirrorY : Point = Point(- x , y)
    def flipCoords: Point = Point(y, x)
    def -(move: Move): Point = Point(x - move.x, y - move.y)
    def unary_- : Point = Point(-x, -y)
    def compare(that: Point): Int = (this.x, this.y).toString() compare (that.x, that.y).toString()
  }

  type Move = Point

  case class Board(cells: List[List[String]], forbiddenValues: List[String]){
    def issInside(point: Point): Boolean = {
      point.y <= 0 &&
        point.x >= 0 &&
        -point.y < cells.length &&
        point.x < cells(-point.y).length &&
        !forbiddenValues.contains(cells(-point.y)(point.x))
    }

    /**
      * given in mathematical coordinates  from top left (x always positive, y always negative)
      * @param x
      * @param y
      * @return
      */
    def valueOf(x: Int, y: Int): String = cells(-y)(x)

    def valueOf(point: Point): String = this.valueOf(point.x, point.y)

    def getPoints: List[Point] = {
      var points: List[Point] = List();
       for (matrixY <- 0 to cells.length-1){
         val row = cells(matrixY)
         for (matrixX <- 0 to row.length-1 ){
           val point = Point(matrixX, -matrixY)
           if (!forbiddenValues.contains((valueOf(point)))) points = points ::: List(point)
         }
       }
      points
    }
  }


  def moves(board: Board, origin: Point, movAbility: MovAbility): List[Point] = {

    var vectors = List(movAbility.vector);
    if (movAbility.coordFlip) vectors = vectors ::: vectors.map(vec=> vec.flipCoords)
    if (movAbility.mirrorX)   vectors = vectors ::: vectors.map(vec=> vec.mirrorX)
    if (movAbility.mirrorY)  vectors = vectors ::: vectors.map(vec=> vec.mirrorY)
    if (movAbility.centralSym)  vectors = vectors ::: vectors.map(vec=> -vec)

    var res: List[Point] = List()
    for (vec <- vectors){
      res = res ::: applyVectorToPoint(board, origin, vec, movAbility.repeatable)
    }
    res
  }


  //  def applyVectorToList(list: List[Point], board: Board, origin: Point, vector: Move, repeatable: Boolean): List[Point] = {
  //    val twoDlist = for(point <- list) yield applyVectorToPoint(board, point, vector, repeatable);
  //    twoDlist.flatten
  //  }

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


  case class MovAbility(vector: Point, repeatable: Boolean, coordFlip: Boolean,  mirrorX: Boolean, mirrorY: Boolean, centralSym: Boolean );


  case class Piece (name: String, movAbilities: List[MovAbility]) {
    def getDestinations(board: Board, origin: Point): List[Point] = {
      val twoDList = for (elem <- this.movAbilities) yield moves(board, origin, elem)
      twoDList.flatten
    }
  }

  def superLevel(board: Board, piece: Piece, initial: List[PointNode], canRepeat: Boolean): List[PointNode]= {
    var parents: List[PointNode] = Nil
    for (node <-initial){
      //one move backwards
      var destinations = piece.getDestinations(board, node.value)
      if (!canRepeat) destinations = destinations.filter(point=> node.allowedAsParentForNonRepeatables(point) )
      destinations.foreach(dest => {
        val parentNode = node.createParent(dest)
        parents = parents ::: List(parentNode)
      })
    }
    parents = parents.sortBy(_.value)
    parents = mergeEqualValues(parents)
    parents
  }

  def superLevelOrder(board: Board, piece: Piece, initial: List[PointNode], canRepeat: Boolean, order: Int):  List[PointNode] ={
    var parents = initial;
    for (level <- 0 to order-1){
      parents = superLevel(board, piece, parents, canRepeat);
    }
    parents
  }



  def mergeEqualValues (nodeList: List[PointNode]): List[PointNode] = nodeList match {
    case Nil => Nil
    case h :: Nil => h :: Nil
    case h :: x :: t => if (h.value == x.value)  List(h.merge(x)) ::: mergeEqualValues(t)  else h ::  mergeEqualValues(x :: t)
  }

  case class PointNode(value: Point, childrenLists: List[List[PointNode]]) {

    def createParent(point: Point): PointNode ={
      val listsWithPrefix: List[List[PointNode]] =  this.childrenLists match {
        case Nil => List(List(this))
        case _ => this.childrenLists.map(list => List(this) ::: list)
      }
      val dryLists = listsWithPrefix.map(li => li.map(pn => pn.withoutList()))
      PointNode(point, dryLists)
    }

    def allowedAsParentForNonRepeatables(point: Point): Boolean = {
      if (value == point)  false
      else if (childrenLists == Nil)  true
      else !childrenLists.flatten.map(node => node.value).contains(point)
    }

    def merge(node: PointNode): PointNode = {
      if (this.value != node.value ) throw new IllegalArgumentException("nodes with different values can't be merged");
      return new PointNode(this.value, this.childrenLists ::: node.childrenLists);
    }

    def withoutList(): PointNode ={
      PointNode(this.value, Nil)
    }
  }



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



  def createCleanSquareBoard (side: Int): List[List[String]] = {
    val vec =for (y <- 0 to side-1) yield List.tabulate(side)(x=> f"$x $y")
    vec.toList
  }


  val bishopMoves = MovAbility(Point(1, -1), true, false, true, true, false)




  val bishop = Piece("bishop", List(bishopMoves))

  val threeBoard = Board(createCleanSquareBoard(3), List())

  val keyPad = Board( List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"), List("*", "0", "#")), List("*", "#"))

  val knightMov = MovAbility(Point(1, 2), false, true, true, false, true)
  val knight = Piece("knight", List(knightMov))

  def main(args: Array[String]): Unit = {
//    var tree9 = new PointsNode(Point(2, -2))
//    tree9.populate(keyPad, knight, 7)
//    println("starting from 9=" + tree9.countLeaves)
//    println(tree9.debugLeftBranch())





  }










}
