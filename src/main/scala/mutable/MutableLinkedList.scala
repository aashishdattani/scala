package mutable

import scala.annotation.tailrec
import scala.collection.mutable.StringBuilder

class MutableLinkedList[T](var headOption : Option[T], var tailOption : Option[MutableLinkedList[T]]) {
  def isEmpty: Boolean = headOption match {
    case Some(_) => false
    case None => true
  }

  def head: T = headOption match {
    case None => throw new NoSuchElementException
    case Some(x) => x
  }

  def tail: MutableLinkedList[T] = tailOption match {
    case None => throw new NoSuchElementException
    case Some(x) => x
  }

  def prepend(element: T): Unit = {
    headOption match {
      case Some(_) => tailOption = Some(new MutableLinkedList(headOption, tailOption))
      case None =>
    }
    headOption = Some(element)
  }

  @tailrec
  final def exists(element: T): Boolean = (headOption, tailOption) match {
    case (Some(h), _) if h == element => true
    case (_, Some(t)) => t.exists(element)
    case (_, _) => false
  }

  @tailrec
  final def delete(element: T): Unit = (headOption, tailOption) match {
    case (Some(h), _) if h == element =>
      tailOption match {
        case None =>
          headOption = None
        case Some(x) =>
          headOption = x.headOption
          tailOption = x.tailOption
      }
    case (_, Some(t)) => t.delete(element)
    case (_, _) => throw new NoSuchElementException
  }

  override def toString: String = {
    @tailrec
    def toStringHelper(list:MutableLinkedList[T], current:StringBuilder): String = { 
      def append(value: String): Unit = {
        if (!current.isEmpty)
             current += ','
         current ++=  value
      }
      (list.headOption, list.tailOption) match {
        case (None, _) => 
          current.toString
        case (Some(h), None) => 
          append(h.toString)
          current.toString
        case (Some(h), Some(t)) => 
          append(h.toString)
          toStringHelper(t, current)
      }
    }
    toStringHelper(this, new StringBuilder()) 
  }
}

object MutableLinkedList {
  def apply[T](values: T*): MutableLinkedList[T] = values.size match {
    case 0 => new MutableLinkedList(None, None)
    case 1 => new MutableLinkedList(Some(values.head), None)
    case _ => new MutableLinkedList(Some(values.head), Some(apply(values.tail: _*)))
  }
}
