package recfun

object Main {
  def main(args: Array[String]) {


    //
    println("Pascal's Triangle")
    pascal(1, 3)
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

  }


  /**
   * Exercise 1
   */
  def pascal(position: Int, row: Int): Int = {
    if (position == 0 || position == row) 1 else {
      val upperRow = row -1
      val parentLeft = pascal(position-1, upperRow)
      val parentRight = pascal(position, upperRow)
      parentLeft + parentRight
    }
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {


    /**
      * extracts the first valid closed parenthesis group from a string, starting from the left
      * @param chars
      * @return the position of the last character of the left substring + 1
      *         or -1 if no parentheses found
      *         or -2 if unbalanced parentheses
      */
    def extractValid(chars: List[Char]) : Int = {

      /**
        * @param chars
        * @return the position of the first open parenthesis char
        *         -1 if none found
        *         -2 if the first thing found is a closed parenthesis instead
        */
      def firstOpen(chars: List[Char]): Int ={
        var ret =  -1
        var position = 0
        while (ret == -1 && position < chars.size ){
          val current = chars(position)
          if (current == '(' ) ret = position
          else if  (current == ')') ret = -2
          else  position +=1
        }
        ret
      }



      val firstOp = firstOpen(chars)
      if (firstOp == -1 || firstOp == -2) return firstOp
      var start = firstOp + 1
      var acc = 1
      do{
        val current = chars(start)
        if (current == '(') acc +=1
        if (current == ')') acc -=1
        start +=1
      }while(start < chars.size && acc !=0)
      if (acc == 0) start else -2
    }




    val endValid = extractValid(chars)
    if (endValid == -1) true
    else if (endValid == -2) false
    else if (endValid == chars.size) true
    else balance(chars.drop(endValid))
  }
  
  /**
   * Exercise 3
   */
  def countChange(amount: Int, denominations: List[Int]): Int = {


    if (denominations.isEmpty){
      return 0;
    }

    var matches = 0

    /**
      * given an amount and the minimum coin value, recursively
      * determines how many exact matches can be obtained by
      * successively resting a coin to the reminder.
      *
      * The "tree" of possible coin values is limited so that
      * the parent is always the minimum value that can be chosen as child.
      *
      * In this way,
      * no tree paths are repeated if order is not considered.
      * @param amount
      * @param minimumCoin
      */
    def possibleRests(amount: Int, minimumCoin: Int): Unit ={
      var coins =  possibleCoins(amount, minimumCoin)
      while (coins.nonEmpty){
        val coin = coins.head
        coins = coins.tail
        if (coin == amount) matches +=1
        else if (coin < amount){
          val rest= amount - coin
          possibleRests(rest, coin)
        }
      }
    }

    /**
      * helps create a tree of coin denominations that can be applied to an original money amount.
      * Each new node of the tree is a "rest" (change), to which again coins are applied
      * The children of each node are limited by 2 things:
      * - children smaller than the parent are not allowed. This is done so that no "coin paths" are repeated, if one disregards order
      * - children bigger than the rest(change) also are not allowed
      *
      * given an amount and the minimum coin denomination I can use (from within a list of coin denominations)
      * returns what coins can be applied individually to that amount to obtain a rest
      * @param amount
      * @param minimumCoin
      * @return
      */
    def possibleCoins(amount: Int, minimumCoin: Int): List[Int] = {
      /**
        * filters the list of denominations, so that the lowest value is at least the one indicated
        * @param min
        * @return  the shortened list
        */
      def reduceByMin(min: Int): List[Int] ={
        var pieces = denominations.sorted
        var ate = false
        do{
          ate = false
          if ( min > pieces.head ) {
            pieces = pieces.tail
            ate = true
          }
        }while(pieces.nonEmpty && ate )
        pieces
      }

      /**
        * filters a sub-list of denominations, so that no value is bigger than the amount specified
        * @param max the maximum amount allowed for any denomination (represents the money rest or change)
        * @param startValues a sub-list of denominations
        * @return
        */
      def reduceByMax(max: Int, startValues: List[Int] ) : List[Int] ={
        var pieces = startValues.sorted.reverse
        var ate = false
        do{
          ate = false
          if ( max < pieces.head ) {
            pieces = pieces.tail
            ate = true
          }
        }while(pieces.nonEmpty && ate )
        pieces.reverse
      }

      val byMin = reduceByMin(minimumCoin)
      reduceByMax(amount, byMin)
    }

    possibleRests(amount, 0)
    matches
  }
  }
