import org.scalatest.FunSuite

class SetSuite extends FunSuite {

  trait Sample {
  }

  test("An empty Set should have size 0") {
    new Sample {
      assert(Set.empty.size == 0)
    }
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }

  test("A real test for plying with hkt") {

  }

}
