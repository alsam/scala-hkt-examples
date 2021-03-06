package hkt 

import scala.language.higherKinds

trait Functor[F[_]] {
  def fmap[T, U](m: F[T])(fn: (T) => U): F[U]
}

object Functor {
  def apply[F[_]](implicit f: Functor[F]) = f
}

trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](fa: => F[A])(f: => F[A => B]): F[B] // its synonim ia a <*>

  def <*>[A,B](fa: => F[A])(f: => F[A => B]): F[B] = apply(fa)(f)

  def point[A](a: => A): F[A]

  def pure[A](a: => A): F[A] = point(a)

}

object Applicative {
  def apply[F[_]](implicit a: Applicative[F]) = a
}
