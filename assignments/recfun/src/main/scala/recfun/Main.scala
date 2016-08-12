package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(count: Int, chars: List[Char]): Boolean = {
      if (count < 0) false
      else if (chars.isEmpty) count == 0
      else {
        chars.head match {
          case '(' => balanceHelper(count + 1, chars.tail)
          case ')' => balanceHelper(count - 1, chars.tail)
          case _ => balanceHelper(count, chars.tail)
        }
      }
    }
    balanceHelper(0, chars);
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeHelper(preCount: Int, money: Int, coins: List[Int]): Int = {
      if (money < 0) preCount
      else if (coins.isEmpty) preCount
      else if (money == coins.head) preCount + 1
      else preCount + countChangeHelper(0, money - coins.head, coins) + countChangeHelper(0, money, coins.tail)
    }

    countChangeHelper(0, money, coins.sorted)
  }
}
