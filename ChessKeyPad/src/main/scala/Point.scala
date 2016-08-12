/**
  * Created by gonzalodiaz on 8/10/16.
  */
case class Point(x: Int, y: Int) extends Ordered[Point]{
  def +(move: Point): Point = Point(move.x + x, move.y + y)
  def mirrorX  : Point = Point(x, - y)
  def mirrorY : Point = Point(- x , y)
  def flipCoords: Point = Point(y, x)
  def -(move: Point): Point = Point(x - move.x, y - move.y)
  def unary_- : Point = Point(-x, -y)
  def compare(that: Point): Int = (this.x, this.y).toString() compare (that.x, that.y).toString()
}


