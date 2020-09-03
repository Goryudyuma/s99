package Problems

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.language.implicitConversions

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

  sealed trait ListAndValue[A]

  case class ListAndValueValue[A](value: A) extends ListAndValue[A]

  case class ListAndValueList[A](list: List[ListAndValue[A]]) extends ListAndValue[A]

  implicit def fromValueToListAndValueValue[A: ClassTag](value: A): ListAndValue[A] = {
    value match {
      case list: List[A@unchecked] => fromListToListAndValueList(list)
      case one => ListAndValueValue[A](one)
    }
  }

  implicit def fromListToListAndValueList[A: ClassTag](list: List[A]): ListAndValue[A] = {
    ListAndValueList[A@unchecked](list.map(fromValueToListAndValueValue))
  }

  implicit def fromListAndValueValueToValue[A: ClassTag](value: ListAndValueValue[A]): A = {
    value.value
  }

  def flatten[A](list: ListAndValue[A]): List[A] = {
    list match {
      case ListAndValueValue(v) => List(v)
      case ListAndValueList(list: List[ListAndValue[A]]) =>
        reduceLeft[List[A], List[A]](a => b => a ++ b)(map[ListAndValue[A], List[A]](flatten[A])(list))
    }
  }

  @tailrec
  def reduceLeft[A, B >: A](f: B => A => B)(list: List[A], prev: Option[B] = None): B = {
    (list.isEmpty, prev) match {
      case (true, None) => throw new java.lang.UnsupportedOperationException("empty.reduceLeft")
      case (true, Some(ans)) => ans
      case (false, None) => reduceLeft[A, B](f)(list.tail, Some(list.head))
      case (false, Some(one)) => reduceLeft(f)(list.tail, Some(f(one)(list.head)))
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

  def encodeModified[A](list: List[A]): List[Any] = {
    (map[(Int, A), Any] _) ({
      case (num: Int, c) => if (num == 1) c else (num, c)
    }: ((Int, A)) => Any)(encode(list): List[(Int, A)])
  }

  @tailrec
  def fillList[A](num: Int, one: A, ret: List[A] = List()): List[A] = {
    if (num == 0) ret
    else fillList(num - 1, one, one :: ret)
  }

  def decode[A](list: List[(Int, A)]): List[A] = {
    list.flatMap { case (num, one) => fillList(num, one) }
  }


  @tailrec
  def encodeDirect[A](list: List[A], prev: Option[(Int, A)] = None, ans: List[(Int, A)] = List()): List[(Int, A)] = {
    if (list.isEmpty) {
      reverse(
        if (prev.isDefined) prev.get +: ans
        else ans
      )
    }
    else if (prev.isEmpty) encodeDirect(list.tail, Some((1, list.head)), ans)
    else {
      val prevValue: (Int, A) = prev.get
      if (prevValue._2 == list.head)
        encodeDirect(list.tail, Some((prevValue._1 + 1, list.head)), ans)
      else
        encodeDirect(list.tail, Some((1, list.head)), prevValue +: ans)
    }
  }

  def duplicate[A](list: List[A]): List[List[A]] = {
    flatten[List[A]](map[A, List[A]](a => List(a, a))(list))
  }

  def duplicateN[A](num: Int, list: List[A]): List[List[A]] = {
    flatten[List[A]](map[A, List[A]](a => List.fill(num)(a))(list))
  }

  @tailrec
  def drop[A](num: Int, list: List[A], ans: List[A] = List()): List[A] = {
    if (list.isEmpty) ans
    else {
      val (front, back) = split(num, list)
      val ret = ans ++ front.take(num - 1)
      drop(num, back, ret)
    }
  }

  @tailrec
  def split[A](num: Int, list: List[A], ans: List[A] = List()): (List[A], List[A]) = {
    if (num == 0) (reverse(ans), list)
    else if (list.isEmpty) (reverse(ans), list)
    else split(num - 1, list.tail, list.head :: ans)
  }

  def slice[A](begin: Int, end: Int, list: List[A]): List[A] = {
    split(begin, split(end, list)._1)._2
  }
}