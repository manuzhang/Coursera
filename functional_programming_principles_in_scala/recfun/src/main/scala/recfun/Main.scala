package recfun
import common._

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
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    aux(chars, List())
  }

  def aux(chars: List[Char], stack: List[Char]): Boolean = {
    if (chars.isEmpty && stack.isEmpty) true
    else if (chars.isEmpty && !stack.isEmpty) false
    else if (chars.head == '(') aux(chars.tail, '(' +: stack)
    else if (chars.head == ')' && stack.isEmpty) false
    else if (chars.head == ')' && stack.head == '(') aux(chars.tail, stack.tail)
    else aux(chars.tail, stack)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
