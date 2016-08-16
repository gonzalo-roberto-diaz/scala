/**
  * A bi-dimensional board of String values, represented by a List of Lists. The "forbidden" values can't be the
  * destination of any movement.
  */
case class Board(cells: List[List[String]], forbiddenValues: List[String]){
  def issInside(point: Point): Boolean = {
    point.y <= 0 &&
      point.x >= 0 &&
      -point.y < cells.length &&
      point.x < cells(-point.y).length &&
      !forbiddenValues.contains(cells(-point.y)(point.x))
  }

  /**
    * The String value of a given cell, given in mathematical coordinates assuming that the
    * top left is (0, 0). This means that x should always be positive, and y should always be negative.
    * @param x    the x axis value, corresponding to the index of a given list
    * @param y    the y axis value, corresponding to the index of the enclosing List[List], starting from the top at 0
    *             and increasing as the parameter becomes more negative, i.e., the second list is -1 and so on
    * @return     the string value, or "label" of that board cell
    */
  def valueOf(x: Int, y: Int): String = cells(-y)(x)

  def valueOf(point: Point): String = this.valueOf(point.x, point.y)

  /**
    * @return the geometrical Points of the list of lists, less those containing "forbidden" values
    */
  def getPoints: List[Point] = {
    var points: List[Point] = List()
    for (matrixY <- cells.indices){
      val row = cells(matrixY)
      for (matrixX <- row.indices ){
        val point = Point(matrixX, -matrixY)
        if (!forbiddenValues.contains(valueOf(point))) points = points ::: List(point)
      }
    }
    points
  }
}
