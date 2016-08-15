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
    * @param x
    * @param y
    * @return
    */
  def valueOf(x: Int, y: Int): String = cells(-y)(x)

  def valueOf(point: Point): String = this.valueOf(point.x, point.y)

  /**
    * @return the geometrical Points of the list of lists, less those containing "forbidden" values
    */
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
