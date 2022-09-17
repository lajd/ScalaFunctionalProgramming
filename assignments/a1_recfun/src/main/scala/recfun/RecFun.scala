package recfun

import scala.collection.mutable.{Stack, Map}

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    /* Compute the pascal value of (col, row)
      //    1
      //   1 1
      //  1 2 1
      // 1 3 3 1
      //1 4 6 4 1
      // Examples:
      // pascal(0, 0) -> 1
      // pascal(0, 1) -> 1
      // pascal(1, 1) -> 1
      // pascal(1, 2) -> 2
      // pascal(1, 3) -> 3
      // pascal(2, 4) -> 6
      // ...

    Approach: The value at a given (col, row) pair is determined
      by summing the parent values. Note that memoization provides
      a significant performance improvement here, since otherwise,
      the same values are are recomputed multiple times.
    */

    def _pascal(c: Int, r: Int, memo: Map[(Int, Int), Int] = Map()): Int = {
      if (memo.contains((c, r))) {
        memo((c, r))
      } else {
        // numCols = numRows
        if (c > r) {
          throw new java.lang.IllegalArgumentException("c cannot be > r.")
        } else if (c == 0 && r == 0) {
          // Base case; first element
          1
        } else if (c == 0 || c == r) {
          // Edge case, always 1
          1
        } else {
          // Interior case
          // Recursively decrement c, r
          val resLeft = memo.getOrElse((c - 1, r - 1), default=_pascal(c - 1, r - 1, memo))
          memo((c - 1, r - 1)) = resLeft

          val resRight = memo.getOrElse((c, r - 1), default=_pascal(c, r - 1, memo))
          memo((c, r - 1)) = resRight

          memo((c, r)) = resLeft + resRight
          resLeft + resRight
        }
      }

    }
    _pascal(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    /* Determine if parenthases are balanced in a string expression

    Approach: Differentiate between open and closed brackets,
      maintaining a mapping between an open brack and its corresponding
      close bracket. For the enxt character, identify if it's an opening
      or a closing bracket. If it's an opening bracket, add it to a stack.
      If it's a closing bracket, then it must correspond to the previous
      opening bracket added to the stack. If it does, the we pop the opening
      bracket from the stack and continue. If it does not, then we cannot have
      balance.

      Increment through all the characters, and at the end, check if the stack is empty.
      If the stack is empty, then we have balanced parentheses.
    */

    val openClosedMap = Map(('(', ')'))
    val openBrackets: Set[Char] = Set.from(openClosedMap.keys)
    val closedBrackets: Set[Char] = Set.from(openClosedMap.values)

    def _is_balanced(charList: List[Char], stack: Stack[Char]): Boolean = {
      if (charList.isEmpty) {
        if (stack.isEmpty) {
          return true
        } else {
          return false
        }
      }

      val (next, remaining) = (charList.head, charList.tail)

      if (openBrackets.contains(next)) {
        // Add to stack
        stack.push(next)
      } else if (closedBrackets.contains(next)) {
        // It must be the same as the complement of the last open bracket on the stack
        if (stack.isEmpty) {
          return false
        }

        val last = stack.pop() // Open bracket
        val complement = openClosedMap(last) // Closed bracket complement
        if (complement != next) {
          return false
        }
      }
      // Tail recursion
      return _is_balanced(remaining, stack)
    }
    return _is_balanced(chars, new Stack[Char]())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    /* Count the number of combinations one can obtain `m` from `c`

    Approach: We decompose this problem into subproblems and make use of recursion.
      In particular, we have two subproblems:
        - How many combinations of ways can we produce `m` using
          the n'th coin?
          -> i.e., countChange(m - c[n], c)
          -> Note that we subtract c[n] from m to indicate
            that we are using this coin at least once
        - How many combinations of ways can we produce `m` without
          using the n'th coin?
          -> i.e., countChange(m, c - c[n])
          -> Note that we remove the n'th coin from c since
            we are not using this coin at all
      There are 2 base cases
        - When m === 0, combinations = 1
          - In this case, we use no c to produce m=0
        - When m > 0, len(c) === 0, combinations = 0
          - In this case, we have no c to make up m,
            and so no configurations
      We can use recursion to continue splitting into subproblems.

    Finally, we memoize this problem for performance.
    */

    def _countChange(m: Int, c: List[Int], memo: Map[(Int, List[Int]), Int] = Map()): Int = {
      if (memo.contains((m, c))) {
        memo((m, c))
      } else {
        // Base cases
        if (m == 0) {
          1
        } else if (c.isEmpty || m < 0) {
          0
        } else {
          val withCoinCount = memo.getOrElse((m - c.head, c), default = _countChange(m - c.head, c, memo))
          memo((m - c.head, c) ) = withCoinCount

          val withoutCoinCount = memo.getOrElse((m, c.tail), default = _countChange(m, c.tail, memo))
          memo((m, c.tail)) = withoutCoinCount

          memo((m, c)) = withCoinCount + withoutCoinCount
          withCoinCount + withoutCoinCount
        }
      }
    }
    _countChange(money, coins)
  }
