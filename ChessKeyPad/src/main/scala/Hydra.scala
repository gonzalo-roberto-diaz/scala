/**
  * A way to represent multiple, optional heads sharing the same tail
  */
case class Hydra(heads: List[Point], tail: List[Point]) {

  def applyPoints(points: List[Point], withRepetition: Boolean): List[Hydra] = {
    var addenda = points
    if (!withRepetition) {
      addenda = addenda diff heads
      addenda = addenda diff tail
    }
    for(head <- heads)
        yield Hydra (addenda,  head :: tail)
  }

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
