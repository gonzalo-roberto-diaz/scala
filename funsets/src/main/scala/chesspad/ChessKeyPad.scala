package chesspad

/**
  * Created by Gonzalo on 7/29/2016.
  */
object ChessKeyPad {

  private[chesspad] val keyPad = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(0) )

  case class Point(val x: Int, val y: Int)  {
    def inside: Boolean = (y < keyPad.length ) && (x <  keyPad(y).length)
  }

  case class Movement(val vector: Point){
    def valid(from: Point): Boolean =  apply(from).inside
    def apply(point: Point): Point = new Point(point.x + vector.x, point.y + vector.y)
    def repeatable = false
  }

  class MultiMovement (vector: Point) extends Movement(vector) {
    override def repeatable = true;
    def timesValid(from: Point): Int = {
      def validAcc(point: Point, acc: Int): Int ={
        if (!valid(point)) acc
        else validAcc(apply(point), acc + 1)
      }
      validAcc(from, 0);
    }
  }

  val knightMov = Movement(Point(1, 2))



  def main(args: Array[String]): Unit = {
    println(keyPad(2).length)
  }








}
