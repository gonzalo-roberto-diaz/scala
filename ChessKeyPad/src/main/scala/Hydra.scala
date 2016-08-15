
/**
  * A way to represent multiple, optional List heads sharing the same tail. This is useful
  * to somewhat efficiently store movement paths that differ only in the first element.
  */
case class Hydra(heads: List[Point], tail: List[Point]) {

  type Move = Point

  /**
    * Finds out the derived paths (in Hydra format), that result from applying any from a given set of vectors to
    * each of the elements of the Hydra's head as starting point.
    * @param vectors          the vectors that originate in that point
    * @param withRepetition   whether or not repeated points values are allowed in a path
    * @return                 a list of derivate hydras
    */
  def applyPoints(vectors: List[Move], withRepetition: Boolean): List[Hydra] = {
    var addenda = vectors
    if (!withRepetition) {
      addenda = addenda diff heads
      addenda = addenda diff tail
    }
    for(head <- heads)
        yield Hydra (addenda,  head :: tail)
  }

  /**
    * Dumps a hydra in terms of a specific board's point values, rather than
    * the default dump in terms of the points themselves.
    * @param board
    * @return
    */
  def toString(board: Board): String ={

    def listToString(points: List[Point]): String ={
      val buf = new StringBuilder()
      buf.append("(")
      for (h <- points) buf.append(board.valueOf(h)).append(", ")
      if (points.nonEmpty) buf.delete(buf.size-2, buf.size)
      buf.append(")")
      buf.toString()
    }
    listToString(this.heads) + listToString(this.tail)
  }
}
