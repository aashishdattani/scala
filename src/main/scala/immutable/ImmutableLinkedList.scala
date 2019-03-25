package immutable

import scala.annotation.tailrec

sealed trait ImmutableLinkedList[+T] {

  def ::[U >: T](value: U) = NonEmptyList(value, this)

  def :::[U >: T](that: ImmutableLinkedList[U]) : ImmutableLinkedList[U] = {
    that match {
      case EmptyList => this
      case NonEmptyList(head, tail) => NonEmptyList(head, tail ::: this)
    }
  }

  @tailrec
  final def foldLeft[V, U >: T](current: V)(f: (V, U) => V): V = {
    this match {
      case EmptyList => current
      case NonEmptyList(head, tail) => tail.foldLeft(f(current, head))(f)
    }
  }

  def reverse(): ImmutableLinkedList[T] =
    foldLeft(ImmutableLinkedList[T]())((current, head) => head :: current)

  def foldRight[V, U >: T](current: V)(f: (U, V) => V): V =
    reverse().foldLeft(current)((current, head) => f(head, current))

  def map[U >: T, V](f : U => V): ImmutableLinkedList[V] =
    foldRight(ImmutableLinkedList[V]())((head, current) => f(head) :: current)

  override def toString: String = {
    this match {
      case EmptyList => ""
      case NonEmptyList(head, EmptyList) => head.toString
      case NonEmptyList(head, tail) => head.toString + "," + tail.toString
    }
  }
}

case class NonEmptyList[+T](head: T, tail: ImmutableLinkedList[T])
  extends ImmutableLinkedList[T]

case object EmptyList extends ImmutableLinkedList[Nothing]

object ImmutableLinkedList {

  def apply[T](values: T*) : ImmutableLinkedList[T] = {
    if (values.isEmpty)
      EmptyList
    else
      NonEmptyList(values.head, apply(values.tail: _*))
  }
}