package Problems

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkingWithListsSpec extends AnyFlatSpec with Matchers {
  "P01" should "例ケース" in {
    WorkingWithLists.last(List(1, 1, 2, 3, 5, 8)) shouldEqual 8
  }
  it should "Stringの場合" in {
    WorkingWithLists.last(List("1", "1", "2", "3", "5", "8")) shouldEqual "8"
  }
  it should "空の場合" in {
    assertThrows[java.util.NoSuchElementException] {
      WorkingWithLists.last(List(): List[Int])
    }
  }

  "P02" should "例ケース" in {
    WorkingWithLists.penultimate(List(1, 1, 2, 3, 5, 8)) shouldEqual 5
  }
  it should "listの長さが1の場合" in {
    assertThrows[java.util.NoSuchElementException] {
      WorkingWithLists.penultimate(List(1): List[Int])
    }
  }

  "P03" should "例ケース" in {
    WorkingWithLists.nth(2, List(1, 1, 2, 3, 5, 8)) shouldEqual 2
  }
  it should "0番目" in {
    WorkingWithLists.nth(0, List(1, 1, 2, 3, 5, 8)) shouldEqual 1
  }
  it should "リストの長さより大きい数字を指定" in {
    assertThrows[java.util.NoSuchElementException] {
      WorkingWithLists.nth(6, List(1, 1, 2, 3, 5, 8))
    }
  }
  it should "負の数を指定" in {
    assertThrows[java.util.NoSuchElementException] {
      WorkingWithLists.nth(-1, List(1, 1, 2, 3, 5, 8))
    }
  }

  "P04" should "例ケース" in {
    WorkingWithLists.length(List(1, 1, 2, 3, 5, 8)) shouldEqual 6
  }
  it should "listの長さが1の場合" in {
    WorkingWithLists.length(List(1)) shouldEqual 1
  }
}