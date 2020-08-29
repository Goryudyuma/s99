package Problems

import Problems.WorkingWithLists._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkingWithListsSpec extends AnyFlatSpec with Matchers {
  "P01" should "例ケース" in {
    last(List(1, 1, 2, 3, 5, 8)) shouldEqual 8
  }
  it should "Stringの場合" in {
    last(List("1", "1", "2", "3", "5", "8")) shouldEqual "8"
  }
  it should "空の場合" in {
    assertThrows[java.util.NoSuchElementException] {
      last(List(): List[Int])
    }
  }
}