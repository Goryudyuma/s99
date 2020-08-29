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

  "P05" should "例ケース" in {
    WorkingWithLists.reverse(List(1, 1, 2, 3, 5, 8)) shouldEqual List(8, 5, 3, 2, 1, 1)
  }
  it should "listが1だけの場合" in {
    WorkingWithLists.reverse(List(1)) shouldEqual List(1)
  }
  it should "listが空の場合" in {
    WorkingWithLists.reverse(List(): List[Int]) shouldEqual List()
  }

  "P06" should "例ケース" in {
    WorkingWithLists.isPalindrome(List(1, 2, 3, 2, 1)) shouldEqual true
  }
  it should "回文ではない場合" in {
    WorkingWithLists.isPalindrome(List(1, 2, 3, 2, 2)) shouldEqual false
  }

  "P07" should "例ケース" in {
    WorkingWithLists.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldEqual List(1, 1, 2, 3, 5, 8)
  }
  it should "ランダムケース" in {
    WorkingWithLists.flatten(List(1, List(2, List(3, List(4, 5), List(6))), List(7, 8), 9)) shouldEqual
      List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }
}