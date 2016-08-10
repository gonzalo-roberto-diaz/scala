def merge(x: List[Int], y: List[Int]): List[Int] =  (x, y) match{
  case (Nil, y) => y
  case (x, Nil) => x
  case (hx :: tx, hy :: ty) => if (hx < hy) hx :: merge(tx, y) else hy :: merge(x, ty)
}

val a = List(4, 6, 8)
val b = List(1, 3, 5)

merge(a, b)
