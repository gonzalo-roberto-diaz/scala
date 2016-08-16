
/**
  * A way to represent multiple, optional List heads sharing the same tail. This is useful
  * to somewhat efficiently store movement paths that differ only in the first element.
  */
case class Hydra[T](heads: List[T], tail: List[T]) {


  /**
    * Finds out the derived paths (in Hydra format), that result from applying any from a given set of vectors to
    * each of the elements of the Hydra's head as starting point.
    * @param vectors          the vectors that originate in that point
    * @param withRepetition   whether or not repeated points values are allowed in a path
    * @return                 a list of derivate hydras
    */
  def applyPoints(vectors: List[T], withRepetition: Boolean): List[Hydra[T]] = {
    var addenda = vectors
    if (!withRepetition) {
      addenda = addenda diff heads
      addenda = addenda diff tail
    }
    for(head <- heads)
        yield Hydra(addenda,  head :: tail)
  }

  /**
    * Dumps a hydra using a function able to transform its individual values into strings
    * @param f    a function that transform an hydra value into its string representation
    * @return     the string representation of a hydra
    */
  def toString(f: T => String): String = {
    def listToString(values: List[T], f: T => String): String ={
      val buf = new StringBuilder()
      buf.append("(")
      for (h <- values) buf.append(f(h)).append(", ")
      if (values.nonEmpty) buf.delete(buf.size-2, buf.size)
      buf.append(")")
      buf.toString()
    }
    listToString(this.heads, f) + listToString(this.tail, f)
  }


  /**
    * Finds out the derivate hydras, based on a function that finds derivate values give a value
    * @param f      the function able to produce derivates
    * @param withRepetition whetner or not the resulting paths/hydras can contain repeated points
    * @return the derivate hydras
    */
  def process(f: T => List[T], withRepetition: Boolean): List[Hydra[T]] ={
    var resHydras: List[Hydra[T]] = Nil
    for (point <- this.heads){
      var destinations = f(point)
      if (!withRepetition) {
        destinations = destinations diff this.heads
        destinations = destinations diff this.tail
      }
      if (destinations != Nil)
        resHydras = resHydras ::: List(Hydra(destinations, point :: this.tail))
    }
    resHydras
  }


  /**
    * repeats the "processing" of an hydra several times, first on the hydra itself, then in its
    * derivates, and so on.
    * @param f                the function able to derivate a value
    * @param withRepetition   whether or not paths/hydras can contain the same point more than once
    * @param times            how many levels we want to calculate
    * @return                 the last result  (not the accumulation) of succesively processing hydras
    */
  def repeatedlyProcess(f: T => List[T], withRepetition: Boolean, times: Int): List[Hydra[T]] ={
    var resHydras: List[Hydra[T]] = List(this)
    for (time <- 1 to times) {
      var partialList: List[Hydra[T]] = Nil
      for (hy <- resHydras) {
        partialList = partialList ::: hy.process(f, withRepetition)
      }
      resHydras = partialList
    }
    resHydras
  }

}
