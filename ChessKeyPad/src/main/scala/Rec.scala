

import scala.collection.mutable.ListBuffer

/**
  * A recursive structure, apt for storing tree paths with "intertrined" branches efficiently.
  * It contains a head or OR-ed elements (any of which can be the head od the path), and
  * a tail containing the rest of the path.
  */
case class Rec[T] (recHead: ListBuffer[T], recTail: ListBuffer[Rec[T]]){

  /**
    * the string representation of a Rec. {} means the head and [] means the tail
    * @return
    */
  override def toString: String ={
    val sb =  StringBuilder.newBuilder

    sb.append("Rec(")

    if (recHead.nonEmpty){
      sb.append("{")
      recHead.foreach(elem => sb.append(elem).append(","))
      sb.delete(sb.size - 1, sb.size)
      sb.append("}")
    }

    if (recTail.nonEmpty) {
      //sb.append(",")
      sb.append("[")
      recTail.foreach(elem => sb.append(elem).append(","))
      sb.delete(sb.size - 1, sb.size)
      sb.append("]")
    }

    sb.append(")")

    sb.toString
  }

  def isUnary: Boolean ={
    recHead.size == 1 && recTail.isEmpty
  }

  /**
    * If there are elements in the list, they have to be unary.
    * Notice that an empty tail returns true
    * @return
    */
  def allUnary: Boolean = {
    recTail.forall(t => t.isUnary)
  }

  def addValuesToTailAsUnaries(values: List[T]): Unit ={
    values.foreach(value => addUnaryToTail(value))
  }

  def addUnaryToTail(value: T): Unit = {
    recTail += Rec(ListBuffer[T](value), ListBuffer())
  }


  /**
    * traverses the tail of this rec, and determines which of the rec elemens of the tail (if any)
    * already contains the exact elements of the list given as a parameter
    * @param tailSought
    * @return the return value or nothing
    */
  def recInTailHavingTail(tailSought: List[T]): Option[Rec[T]] ={
    for (rec <- this.recTail){
      if (rec.recTail.nonEmpty && rec.recTail.map(r => r.recHead.head).toList == tailSought) return Option(rec)
    }
    return Option.empty
  }


  /**
    * enriches one of the unary recs of the tail (the first that it finds)
    * see "enrichTail" for details
    * @param f
    */
  def enrichOne(f: T=> List[T]): Unit ={
    val rec = recTail.filter(rec => rec.isUnary).head
    val derivs = f(rec.recHead.head)
    // if no derivates, delete that rec
    if (derivs.isEmpty) recTail.remove(recTail.indexOf(rec))
    else {
      val recAlreadyWithTail =  recInTailHavingTail(derivs)
      // if that exact tail is not already on some other sibling, just add the values
      if (recAlreadyWithTail.isEmpty) rec.addValuesToTailAsUnaries(derivs)
      // otherwise, add the value to the recVal of the the rec that has that tail, and delete the  unary in question
      else {
        recAlreadyWithTail.get.recHead.append(rec.recHead.head)
        recTail.remove(recTail.indexOf(rec))
      }
    }
  }


  /**
    * using a function that returns derivates from a value, populate the tail of this Rec.
    * The population should not only populate each infividual unary element, but adequately merge them if
    * the returned derivates are repated across unaries
    * This should only apply to recs whose tails is composed all of unaries
    * @param f
    */
  def enrichTail(f: T => List[T]): Unit ={
    if (!allUnary) throw new IllegalArgumentException("Non-unary tails cannot be enriched")
    while(recTail.filter(t => t.isUnary).nonEmpty){
      enrichOne(f)
    }
  }

  /**
    * represents the amount of paths formed.
    * Calculated as the sum of lenght of the heads * tail elements in all last-level recs
    * If the Rec grew evenly (i.e., was properly "enriched"), then the recursion of this function
    * should never reach the unaries.
    * @return
    */
  def cardinality: Int ={
    if (isUnary) throw new IllegalStateException("cardinality cannot be calculated for unary Recs. Unevenly enriched Rec?")
    if (allUnary) recHead.size * recTail.size
    else recTail.map(rec => rec.cardinality).sum
  }

  /**
    * returns the list of all "Last level" Recs, i.e., those recs whose tails elements
    * are all unary. The child unary recs are not "last level".
    * @return
    */
  def lastLevelRecs: List[Rec[T]] ={
    if (allUnary) List(this)
    else{
      val lastOnChildren: ListBuffer[Rec[T]] =  ListBuffer()
        for (rec <- recTail) {
          lastOnChildren.appendAll(rec.lastLevelRecs)
        }
      lastOnChildren.toList
    }
  }

}