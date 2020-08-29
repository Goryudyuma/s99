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
}