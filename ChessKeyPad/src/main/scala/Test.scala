import scala.collection.mutable._


/**
  * Created by gonzalodiaz on 8/13/16.
  */
object Test extends  KeyPadTrait with PointOperations with ChessSpecific with RecOperations[Point]{

  def main(args: Array[String]): Unit ={
    val rec = Rec(ListBuffer[Point](), ListBuffer[Rec[Point]]())

    rec.addValuesToTailAsUnaries(List(kp2, kp3, kp4, kp5, kp6, kp7, kp8, kp9))
    //rec.addValuesToTailAsUnaries(List(kp2))

    val knightFunction: Point => List[Point] = enrichGivenBoardPieceAndPoint(keyPad, knight)

    enrichRepeatedly(rec, knightFunction, 6)

    println(rec)
    println(rec.cardinality)

    val queenFunction: Point => List[Point] = enrichGivenBoardPieceAndPoint(keyPad, queen)
    val recQueen = Rec(ListBuffer[Point](), ListBuffer[Rec[Point]]())

    recQueen.addValuesToTailAsUnaries(List(kp2, kp3, kp4, kp5, kp6, kp7, kp8, kp9))
    enrichRepeatedly(rec, queenFunction, 6)
    println("queen cardinality ="  + recQueen.cardinality)



    //val recsLevel1 = recsLevel(rec, 1)

    //recsLevel1.foreach(rec => rec.enrichTail(knightFunction))

    //println(rec)
  }

  val knightsDerivatesCache: Map[Point, List[Point]] = Map()

  def cachedKinghtFunction(point: Point): List[Point] = {
    val knightFunction: Point => List[Point] = enrichGivenBoardPieceAndPoint(keyPad, knight)
    knightsDerivatesCache.getOrElse(point, knightFunction(point))
  }

  def enrichRepeatedly(rec: Rec[Point], function: Point => List[Point], times: Int ): Unit ={
    var lastLevelRecs: List[Rec[Point]] = Nil

    for (time <- 1 to times) {
      rec.lastLevelRecs.foreach(rec => rec.enrichTail(function))
    }

  }






}
