


import scala.collection.mutable._

def fun[T](): Unit = {
  val leafRecs = Map[Int, ListBuffer[Rec[T]]]()

  val newRec = Rec[T](ListBuffer(), Nil)
  var leavesOfThatIndex = leafRecs.getOrElse(3, ListBuffer())
  leavesOfThatIndex.append(newRec)

}

fun[String]()






