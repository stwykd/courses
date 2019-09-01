
object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    var testBal = "()()()(()("
    println(testBal+balance(testBal.toList).toString)
    testBal = ")()()()(())"
    println(testBal+balance(testBal.toList).toString)
    testBal = "()((((()))))"
    println(testBal+balance(testBal.toList).toString)
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || r == c) 1 else pascal(c, r-1)+pascal(c-1, r-1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], open:Int): Boolean =
        if(open < 0) false
        else if(chars.isEmpty) { if(open == 0) true else false }
        else if(chars.head == '(') loop(chars.tail, open+1)
        else if(chars.head == ')') loop(chars.tail, open-1)
        else loop(chars.tail, open)
      loop(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
        if ((money < 0) || coins.isEmpty) 0
        else if (money == 0) 1
        else countChange(money - coins.head, coins) + countChange(money, coins.tail)
