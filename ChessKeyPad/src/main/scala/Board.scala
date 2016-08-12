/**
  * Created by gonzalodiaz on 8/10/16.
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
    * given in mathematical coordinates  from top left (x always positive, y always negative)
    * @param x
    * @param y
    * @return
    */
  def valueOf(x: Int, y: Int): String = cells(-y)(x)

  def valueOf(point: Point): String = this.valueOf(point.x, point.y)

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
