var board: List[List[String]] = List()

def create7x7Board: List[List[String]] = {
  val vec =for (y <- 0 to 7) yield List.tabulate(7)(x=> f"$x $y")
  vec.toList
}

create7x7Board


