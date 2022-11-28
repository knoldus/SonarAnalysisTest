package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   *  | r *****************
   * c| 0  1  2  3  4  5
   * 0| 1
   * 1| 1  1
   * 2| 1  2  1
   * 3| 1  3  3  1
   * 4| 1  4  6  4  1
   * 5| 1  5 10 10  5  1
   */
  def pascal(c: Int, r: Int): Int = {
    if( r < c || c < 0 || r < 0) 0 //Column index will never be larger than row index for the triangle, and negative indexs make no sense
    else if (c == 0 || r == 0) 1
    else pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def unmatchedBraces(chars:List[Char], previousBraces: List[Char] = List.empty): Boolean = {
      // if chars is empty then we have reached the end
      val preIsEmpty = previousBraces.isEmpty
      if(chars.isEmpty){
        if(preIsEmpty) true
        else false
      }
      else {
        val head = chars.head
        val remainingBody = chars.tail
        if(preIsEmpty)
          unmatchedBraces(remainingBody,List(head))
        else{
          if(previousBraces.head == '(' && head == ')')
            unmatchedBraces(remainingBody,previousBraces.tail)
          else unmatchedBraces(remainingBody,head +: previousBraces)
        }
      }
    }

    if(chars.isEmpty) false //Check if given empty list
    else{
      /**
       * This is also convenient because it handles cases with no braces as
       * unmatchedBraces returns true if the input list is empty
       */
      val braces = chars.filter(ch => ch == '(' || ch == ')')
      unmatchedBraces(braces)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
