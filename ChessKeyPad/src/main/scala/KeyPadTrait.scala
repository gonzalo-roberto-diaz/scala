
/**
  * Created by gonzalodiaz on 8/14/16.
  */
trait KeyPadTrait {

  //keypad Points
  val kp1 = Point(0, 0)
  val kp2 = Point(1, 0)
  val kp3 = Point(2, 0)
  val kp4 = Point(0, -1)
  val kp5 = Point(1, -1)
  val kp6 = Point(2, -1)
  val kp7 = Point(0, -2)
  val kp8 = Point(1, -2)
  val kp9 = Point(2, -2)
  val kp0 = Point(1, -3)

  val keyPad = Board(List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"), List("*", "0", "#")), List("*", "#"))

}
