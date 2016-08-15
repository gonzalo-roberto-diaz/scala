

import scala.collection.mutable.ListBuffer

/**
  * Created by gonzalodiaz on 8/14/16.
  */
trait RecOperations[T] {

  def recsLevel(rec: Rec[T], level: Int): List[Rec[T]] = {
    var accum = List(rec)
    for (lev <- 0 until level){
      accum = accum.flatMap(rec => rec.recTail)
    }
    accum
  }

  def createUnaryRec(value: T): Rec[T] = {
    Rec[T](ListBuffer(value), ListBuffer())
  }

  


}
