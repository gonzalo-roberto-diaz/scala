package test

/**
  * Created by Gonzalo on 7/9/2016.
  */
object Suma extends App{



  def sumFunc (f: Int=> Int, a: Int, b: Int):Int ={
    if (a>b) return 0
    else f(a) + sumFunc(f, a+1, b)
  }

  def prodFunc (f: Int=> Int, a: Int, b: Int):Int ={
    if (a>b) return 1
    else f(a) * prodFunc(f, a+1, b)
  }

  def fac (i: Int): Int ={
    prodFunc(x => x, 1, i)
  }


  def sumFuncCurr (f: Int=> Int) (a: Int, b: Int):Int ={
    if (a>b) return 0
    else f(a) + sumFuncCurr(f)(a+1, b)
  }
//
  val fSumQuad = sumFuncCurr(x => x * x)_
//
  println(fSumQuad(3,4))

  def gen(f: Int=>Int, g: (Int, Int) => Int, neutral: Int) (lower: Int, upper: Int): Int ={
    if (lower > upper) return neutral
    else g( f(lower), gen(f, g, neutral)(lower+1, upper) )
  }

  val sumQuad34 = gen( x=>x*x, (x, y) => x+y, 0)(3,4)

  println(sumQuad34)







//  val res = gen(x => x) (x => x * x) (1, 4)
  //print(4)



}
