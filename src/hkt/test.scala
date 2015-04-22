package hkt

object Test {
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

  def applicative_add_test = {
    val add = (x: Int, y: Int) => x + y

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)

    val result = Applicative[List].<*>(list2)(Functor[List].fmap(list1)(add.curried))
    assert(result == List(5, 6, 7, 6, 7, 8, 7, 8, 9))
    println("applicative_add_test: " + result.toString)
  }

  def applicative_identity_law_test = {
    val expectedList = List(1, 2, 3)
    val outputList = Applicative[List].<*>(expectedList)(List((x: Int) => x))
    assert(expectedList == outputList)
    println("applicative_identity_law_test: " + outputList.toString)
  }

  def applicative_pure_composition_test = {
    val af = Applicative[List]
    val data = 1
    val inc = (x: Int) => x + 1

    val res1 = af.<*>(af.pure(data))(af.pure(inc))
    val res2 = af.pure(inc(data))

    assert(res1 == res2)
    println("applicative_pure_composition_test: " + res1.toString)
  }

  def main(args: Array[String]) {
    println("Starting hkt.Test.Main...")
    println("Passed args are:")
    args foreach println
    applicative_add_test
    applicative_identity_law_test
    applicative_pure_composition_test
  }
}
