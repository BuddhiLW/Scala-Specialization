package recfun

object RecFun extends RecFunInterface:
    // args: Array[String]
  def main(): Unit =
    println("-----------------------")
    println("Pascal's Triangle")
    println("-----------------------")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

    println("-----------------------")
    println("Balanced Parenthesis")
    println("-----------------------")
    val bp1 = "(just an) example"
    val bp2 = "())()"
    println(s"${bp1} @balanced?:  ${balance(bp1.toList)}")
    println(s"${bp2} @not-balanced?:  ${!balance(bp2.toList)}" )

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || (c == 1 && r == 1) || c == 0 || c == r) then
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean =
    // val left = chars.slice(0,chars.length/2)
    // val right = chars.slice(chars.length/2, chars.length)

    def isLeftB(c: Char) =
      c == '('

    def isRightB(c: Char) =
      c == ')'

    def countNext(chars: List[Char], count: Int): Boolean =
      if isLeftB(chars.head) then
        countingBalance(chars.tail, count+1)
      else if isRightB(chars.head) then
        countingBalance(chars.tail, count-1)
      else
        countingBalance(chars.tail, count)


    def countingBalance(chars: List[Char], count: Int): Boolean =
      if (count == 0 && chars.isEmpty) then
        true
      else if (count < 0) then
        false
      else
        countNext(chars, count)

    countingBalance(chars, 0)



  // Exercise 3 -- Helper function, filter valid coins
  // equivalent to filter coins, such that the we take only those coins which coin <= money
  def validCoins(money: Int, coins: List[Int]): List[Int] =
    val sortedCoins: List[Int] = coins.sorted
    val vcs: List[Int] = List() // Valid CoinS (vcs)

    def validCoin(money: Int, coin: Int): Boolean = if (coin <= money) then true else false
    def concatValidCoins(money: Int,
                         sortedCoins: List[Int],
                         validCoins: List[Int]): List[Int] =

      // Sorted list implies that if the head is invalid, then all other coins in the tail will also be.
      if sortedCoins.isEmpty || !validCoin(money, sortedCoins.head) then
        validCoins
      else // if validCoin(money, sortedCoins.head) then
        concatValidCoins(money, sortedCoins.tail, validCoins.concat(List(sortedCoins.head)))

    concatValidCoins(money, sortedCoins, vcs)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    val vcs: List[Int] = validCoins(money, coins) // sorted, increasing order

    def count(money: Int, vcs: List[Int]): Int =
      if money == 0 then
        1
      else if (money > 0 && !vcs.isEmpty) then
        count(money - vcs.head, vcs) + count(money, vcs.tail)
        // count(money - coins.head, coins):
        // is money % coins.head (smalled value) == 0 ?, this recursion line will take care to respond to us
        // as well as it will fold into possible combinations with the other coins

        // count(money, coins.tail):
        // this call will happen for every other coin
        // And in every call it will also fold into the previous case and sum it
      else
        0

    count(money, vcs)
