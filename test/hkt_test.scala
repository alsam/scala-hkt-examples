import org.scalatest.FunSuite

class MySuite extends FunSuite {

  import hkt._

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
   

  trait Sample {
    val id = 0
  }

  test("An empty Set should have size 0") {
    val sample = new Sample {
      assert(Set.empty.size == 0)
    }
    assert(sample.id == 0)
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
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

}
