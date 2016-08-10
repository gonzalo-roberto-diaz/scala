package test

/**
  * Created by Gonzalo on 7/10/2016.
  */
object RationalObj extends App{

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  println(x)

  var s = new Rational(1,3)
  println(s)

  println(x.sub(y).sub(z))




}
