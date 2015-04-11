package hkt 

//
// http://stackoverflow.com/questions/19880207/examples-of-applicative-functor-usage-in-scala
// see also scalaz
// see also algebird of Twitter

trait Functor[F[_]] {
  def fmap[T, U](m: F[T])(fn: (T) => U): F[U]
}

trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](f: F[A => B]): F[A] => F[B]

  def point[A](a: A): F[A]

  def pure[A](a: A): F[A] = point(a)
}

object Applicative {

}

