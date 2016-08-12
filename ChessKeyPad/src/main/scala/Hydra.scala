/**
  * A way to represent multiple, optional heads sharing the same tail
  */
case class Hydra(heads: List[Point], tail: List[Point]){
  def applyPoints(points: List[Point], withRepetition: Boolean): List[Hydra] = {
    var addenda = points
    if (!withRepetition) {
      addenda = addenda diff heads
      addenda = addenda diff tail
    }
    for(head <- heads)
        yield Hydra (addenda,  head :: tail)
  }

}
