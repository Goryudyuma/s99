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

  "P08" should "例ケース" in {
    WorkingWithLists.compress(
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"))) shouldEqual
      List(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"))
  }

  "P09" should "例ケース" in {
    WorkingWithLists.pack(
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"))) shouldEqual
      List(
        List(Symbol("a"), Symbol("a"), Symbol("a"), Symbol("a")),
        List(Symbol("b")),
        List(Symbol("c"), Symbol("c")),
        List(Symbol("a"), Symbol("a")),
        List(Symbol("d")),
        List(Symbol("e"), Symbol("e"), Symbol("e"), Symbol("e")))
  }

  "P10" should "例ケース" in {
    WorkingWithLists.encode(
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"))) shouldEqual
      List(
        (4, Symbol("a")),
        (1, Symbol("b")),
        (2, Symbol("c")),
        (2, Symbol("a")),
        (1, Symbol("d")),
        (4, Symbol("e")))
  }

  "P11" should "例ケース" in {
    WorkingWithLists.encodeModified(
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"))) shouldEqual
      List(
        (4, Symbol("a")),
        Symbol("b"),
        (2, Symbol("c")),
        (2, Symbol("a")),
        Symbol("d"),
        (4, Symbol("e")))
  }

  "P12" should "例ケース" in {
    WorkingWithLists.decode(
      List(
        (4, Symbol("a")),
        (1, Symbol("b")),
        (2, Symbol("c")),
        (2, Symbol("a")),
        (1, Symbol("d")),
        (4, Symbol("e")))) shouldEqual
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"))
  }
  it should "encode & decode" in {
    val initValue = List(1, 1, 2, 3, 3, 4, 4, 4, 5, 6, 6, 6, 6, 7, 8, 9)
    WorkingWithLists.decode(WorkingWithLists.encode(initValue)) shouldEqual (initValue)
  }

  "P13" should "例ケース" in {
    WorkingWithLists.encodeDirect(
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"))) shouldEqual
      List(
        (4, Symbol("a")),
        (1, Symbol("b")),
        (2, Symbol("c")),
        (2, Symbol("a")),
        (1, Symbol("d")),
        (4, Symbol("e")))
  }

  "P14" should "例ケース" in {
    WorkingWithLists.duplicate(
      List(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("d"))) shouldEqual
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("c"),
        Symbol("c"),
        Symbol("d"),
        Symbol("d"))
  }
}