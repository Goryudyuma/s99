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

  "P02" should "例ケース" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldEqual 5
  }
  it should "listの長さが1の場合" in {
    assertThrows[java.util.NoSuchElementException] {
      penultimate(List(1): List[Int])
    }
  }

  "P03" should "例ケース" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) shouldEqual 2
  }
  it should "0番目" in {
    nth(0, List(1, 1, 2, 3, 5, 8)) shouldEqual 1
  }
  it should "リストの長さより大きい数字を指定" in {
    assertThrows[java.util.NoSuchElementException] {
      nth(6, List(1, 1, 2, 3, 5, 8))
    }
  }
  it should "負の数を指定" in {
    assertThrows[java.util.NoSuchElementException] {
      nth(-1, List(1, 1, 2, 3, 5, 8))
    }
  }
}