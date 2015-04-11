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

//
// http://stackoverflow.com/questions/19880207/examples-of-applicative-functor-usage-in-scala
// see also scalaz
// see also algebird of Twitter
//trait GenericFunctor[->>[_, _], ->>>[_, _], F[_]] {
//
//  def fmap[A, B](f: A ->> B): F[A] ->>> F[B]
//}
//
//trait Functor[F[_]] extends GenericFunctor[Function, Function, F] {
//
//  final def fmap[A, B](as: F[A])(f: A => B): F[B] =
//    fmap(f)(as)
//}

//trait Applicative[F[_]] extends Functor[f] {
//
//  def point[A](a: A): F[A]
//
//  def pure[A](a: A): F[A] = point(a)
//
//  def apply[A, B](f: F[A => B]): F[A] => F[B]
//
//  final def apply[A, B](fa: F[A])(f: F[A => B]): F[B] =
//    apply(f)(fa)
//
//  override def fmap[A, B](f: A => B): F[A] => F[B] =
//    apply(pure(f))
//}
//
//object Applicative {
//
//  def pure[A, F[_]](a: A)(implicit applicative: Applicative[F]): F[A] =
//    applicative pure a
//
//  def apply[A, B, F[_]](fa: F[A])(f: F[A => B])(implicit applicative: Applicative[F]): F[B] =
//    applicative.apply(fa)(f)
//
//  implicit object OptionApplicative extends Applicative[Option] {
//
//    override def pure[A](a: A): Option[A] =
//      Option(a)
//
//    override def apply[A, B](f: Option[A => B]): Option[A] => Option[B] =
//      o => for { a <- o; p <- f } yield p(a)
//    }
//}


