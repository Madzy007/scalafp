object MyModule {

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n,1)
  }

  //current is the first number in fibonacci sequence - 0
  //next is the next number in the fibonacci sequence - 1
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, next: Int, current: Int): Int = {
      if (n <= 0) current
      else go(n - 1, next = next + current, current = next)
    }
    go(n, next = 1, current = 0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      n >= as.length -1 || (ordered(as(n), as(n+1)) && loop(n+1))
    }

    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a,b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -2, abs))
    println(formatResult("factorial value", 9, factorial))
    println(formatResult("fibonacci value", 9, fibonacci))
    println(findFirst(Array(1,3,4), (x: Int) => x == 4))
    println(isSorted(Array(1,4,5), (x: Int, y: Int) => x < y))

  }

}
