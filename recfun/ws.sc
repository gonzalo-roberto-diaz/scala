val amount = 1
val denoms = List(25, 10, 5)

//def validPieces(amount: Int, minimumPiece: Int): List[Int] ={
//  var changes = denoms
//  do{
//    if ( amount < changes.head || minimumPiece) {
//      changes = changes.tail
//    }
//  }while(!changes.isEmpty)
//  changes
//}


def reduceByMin(amount: Int): List[Int] ={
  var changes = denoms
  do{
    if ( amount > changes.head) {
      changes = changes.tail
    }
  }while(!changes.isEmpty)
  changes
}

val res = reduceByMin(22)

println(res)



