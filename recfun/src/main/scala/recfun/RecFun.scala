package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if(c == r || c == 0) then 1 else pascal(c-1, r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def checkBalance(chars : List[Char], counter: Int): Boolean ={
      if(chars.isEmpty) {
        counter == 0
      }else{
        chars.head match {
          case '(' => checkBalance(chars.tail, counter + 1)
          case ')' => if (counter > 0) then checkBalance(chars.tail, counter - 1) else false
          case _ => checkBalance(chars.tail, counter)
        }
      }
    }
    checkBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // see Bellman dynamic programming for algorithmic improvement ...
    if (money == 0) {
      1
    }
    else if (money < 0 || coins.isEmpty) {
      0
    }
    else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
