import org.scalatest.FunSuite
import hkt._

class BasicSuite extends FunSuite {

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
  
  implicit val listApplicative: Applicative[List] =
    new Applicative[List] {
      def point[A](a: => A): List[A] = List(a)

      def apply[A,B](fa: => List[A])(f: => List[A => B]): List[B] = for {
        elem <- fa
        func <- f
      } yield func(elem)

      def fmap[A, B](fa: List[A])(f: A => B): List[B] = apply(fa)(point(f))
  }

  test("Functor inc test") {
    def inc(list: List[Int])(implicit func: Functor[List]) = func.fmap(list)(_ + 1)
    assert(inc(List(1, 2, 3)) == List(2, 3, 4))
  }

  test("Functor identity law test") {
    val expectedList = List(1, 2, 3)
    val outputList = Functor[List].fmap(expectedList)(x => x)
    assert(expectedList == outputList)
  }

  test("Functor composition test") {
    val f1 = (x: Int) => x + 1
    val f2 = (x: Int) => x * 2
    val inputList = List(1, 2, 3)

    val compRes = Functor[List].fmap(inputList)(f2 compose f1)
    val mapRes = Functor[List].fmap(Functor[List].fmap(inputList)(f1))(f2)
    
    assert(compRes == mapRes)
  }

  test("Applicative add test") {
    val add = (x: Int, y: Int) => x + y

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)

    val result = Applicative[List].<*>(list2)(Functor[List].fmap(list1)(add.curried))
    assert(result == List(5, 6, 7, 6, 7, 8, 7, 8, 9))
  }

  test("Applicative identity law test") {
    val expectedList = List(1, 2, 3)
    val outputList = Applicative[List].<*>(expectedList)(List((x: Int) => x))
    assert(expectedList == outputList)
  }

  test("Applicative pure composition test") {
    val af = Applicative[List]
    val data = 1
    val inc = (x: Int) => x + 1

    val res1 = af.<*>(af.pure(data))(af.pure(inc))
    val res2 = af.pure(inc(data))

    assert(res1 == res2)
  }

  test("Applicative fmap and <*> test") {
    val af = Applicative[List]
    
    val list = List(1, 2, 3)
    val inc = (x: Int) => x + 1
    
    val res1 = af.fmap(list)(inc)
    val res2 = af.<*>(list)(af.pure(inc))
    
    assert(res1 == res2)
  }

  test("Applicative <*> swap order test") {
    val af = Applicative[List]
    
    val data = 1
    val partialAf = af.pure((x: Int) => x + 1)
    
    val res1 = af.<*>(af.pure(data))(partialAf)
    val res2 = af.<*>(partialAf)(af.pure((func: Int => Int) => func(data)))
    
    assert(res1 == res2)
  }

  test("Applicative lifting functions test") {
    val af = Applicative[List]
    
    val lab: List[Int => String] = List((x: Int) => x.toString)
    val lbc: List[String => Int] = List((x: String) => x.length)
    val list = List(1, 2, 3)
    
    val comp = (bc: String => Int) => (ab: Int => String) => bc compose ab
    
    val res1 = af.<*>( af.<*>(list)(lab) )(lbc)
    val res2 = af.<*>(list)( af.<*>(lab)( af.<*>(lbc)( af.pure(comp) ) ) )
    
    assert(res1 == res2)
  }
}

// a more advanced use case & test suite
class AdvancedSuite extends FunSuite {

  trait Base

  trait MyDsl

  // cake pattern in use
  trait GPU extends Base { self: MyDsl =>

    /** an entity for keeping data on device */
    trait Device[A] /* extends Reifiable[Device[T]] */ {

      /** this is pure from applicative functor, i.e. constructor */
      def to(e: A): Device[A]

      /** kernel launch for device */
      def map[B](fn: A => B): Device[B]
    }

    implicit val deviceFunctor: Functor[Device] = new Functor[Device] {
      def fmap[T, U](m: Device[T])(fn: T => U): Device[U] = m.map(fn)
    }


  }

}
