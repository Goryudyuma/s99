package Problems

import scala.annotation.tailrec

object WorkingWithLists {
  @tailrec
  def last[A](list: List[A]): A = {
    list match {
      case head :: Nil => head
      case _ :: tail => last(tail)
      case Nil => throw new java.util.NoSuchElementException
    }
  }

  @tailrec
  def penultimate[A](list: List[A]): A = {
    list match {
      case head :: _ :: Nil => head
      case _ :: second :: tail => penultimate(second :: tail)
      case _ => throw new java.util.NoSuchElementException
    }
  }

  @tailrec
  def nth[A](num: Int, list: List[A]): A = {
    if (num < 0) throw new java.util.NoSuchElementException
    else if (list.isEmpty) throw new java.util.NoSuchElementException
    else if (num == 0) list.head
    else nth(num - 1, list.tail)
  }

  @tailrec
  def length[A](list: List[A], ans: Int = 0): Int = {
    if (list.isEmpty) ans
    else length(list.tail, ans + 1)
  }

  @tailrec
  def reverse[A](list: List[A], ret: List[A] = List()): List[A] = {
    if (list.isEmpty) ret
    else reverse(list.tail, list.head +: ret)
  }

  def isPalindrome[A](list: List[A]): Boolean = {
    reverse(list) == list
  }

  def flatten(list: List[Any]): List[Any] = {
    list.flatMap {
      case ls: List[_] => flatten(ls)
      case ls => List(ls)
    }
  }
}