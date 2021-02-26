object Chapter2 {

  def main(args: Array[String]): Unit = {
    println("Fib(6) = " + fib(6))
    println("isSorted([4,3,2,1]) = " + isSorted[Int](Array(4,3,2,1), _ > _))
  }

  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go (acc: Int, n: Int): Int =
      if (n < 0) acc
      else go(acc + n, n - 1)
    go(0, n)
  }

  // 2.2
  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.length match {
      case 0 => true
      case 1 => true
      case _ =>
        ordered(as(0), as(1)) match {
          case true => isSorted(as.slice(2, as.length - 1), ordered)
          case false => false
        }
    }
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}