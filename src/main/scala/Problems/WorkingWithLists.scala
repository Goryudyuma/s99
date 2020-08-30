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
    else reverse(list.tail, list.head :: ret)
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

  @tailrec
  def compress[A](list: List[A], ans: List[A] = List()): List[A] = {
    list match {
      case a :: b :: tail if a == b => compress(b :: tail, ans)
      case a :: b :: tail => compress(b :: tail, a :: ans)
      case _ => reverse(list ++ ans)
    }
  }

  @tailrec
  def pack[A](list: List[A], prev: List[A] = List(), ans: List[List[A]] = List()): List[List[A]] = {
    if (list.isEmpty) reverse(prev :: ans)
    else {
      val head = list.head
      if (prev.nonEmpty && prev.head == head || prev.isEmpty) pack(list.tail, head :: prev, ans)
      else pack(list.tail, List(head), prev :: ans)
    }
  }

  def encode[A](list: List[A]): List[(Int, A)] = {
    map((one => (length(one), one.head)): List[A] => (Int, A))(pack(list))
  }

  def map[A, B](f: A => B)(list: List[A]): List[B] = {
    list match {
      case Nil => Nil
      case head :: tail => f(head) :: map(f)(tail)
    }
  }
}