
import org.scalatest._

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors


class MySpec extends UnitSpec {
  // Your tests here
}
