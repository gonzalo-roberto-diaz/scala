package test

/**
  * Created by Gonzalo on 7/10/2016.
  */
class Rational (x: Int, y: Int){
  def numer = x
  def denom = y


  def add(that: Rational): Rational ={
    new Rational((numer * that.denom + that.numer * denom), denom * that.denom)
  }

  override def toString(): String ={
    numer + "/" + denom
  }

  def neg(): Rational ={
    new Rational(numer * -1, denom)
  }

  def sub(that: Rational): Rational ={
    add(that.neg)
  }
}
