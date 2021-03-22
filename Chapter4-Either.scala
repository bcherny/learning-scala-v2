sealed trait Either[+E, +A] {
  // 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield (f(a, bb))
}
case class Left[E](e: E) extends Either[E, Nothing]
case class Right[A](a: A) extends Either[Nothing, A]

object Chapter4 {
  def main(args: Array[String]): Unit = {
    eq(Left(1).map("x"), Left(1))
    eq(Right(1).map(_ * 2), Right(2))

    eq(Left(1).flatMap(_ => Right("x")), Left(1))
    eq(Right(1).flatMap(a => Right(a * 2)), Right(2))
    eq(Right(1).flatMap(a => Left(a * 2)), Left(2))

    eq(Right(1).orElse(Right(2)), Right(1))
    eq(Left(1).orElse(Right(2)), Right(2))
    eq(Right(1).orElse(Left(2)), Right(1))
    eq(Left(1).orElse(Left(2)), Left(2))

    eq(Right(1).map2(Right(2))(_ + _), Right(3))
    eq((Left(1): Either[Int, Int]).map2(Right(2))(_ + _), Left(1))
    eq(Right(1).map2(Left(2): Either[Int, Int])(_ + _), Left(2))
    eq((Left(1): Either[Int, Int]).map2(Left(2): Either[Int, Int])(_ + _), Left(1))
  }

  def eq[A](a: A, b: A) =
    assert(a == b, s"Expected $a to equal $b")
}
