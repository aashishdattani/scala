package mutable

import org.scalatest.FunSuite

class TestMutableLinkedList extends FunSuite  {

  test("initialize") {
    val l = MutableLinkedList(1,2,3)
    assert(l.toString == "1,2,3")
  }

  test("initialize empty") {
    val l = MutableLinkedList()
    assert(l.toString == "")
  }

  test("single") {
    val l = MutableLinkedList(1)
    assert(l.toString == "1")
  }

  test("is empty") {
    val l = MutableLinkedList[Int]()
    assert(l.isEmpty)
    l.prepend(1)
    assert(!l.isEmpty)
    l.prepend(2)
    assert(!l.isEmpty)
  }

  test("prepend") {
    val l = MutableLinkedList(1,2,3)
    l.prepend(4)
    assert(l.toString == "4,1,2,3")
  }

  test("exists") {
    val l = MutableLinkedList(1,2,3)
    assert(l.exists(2))
  }

  test("exists invalid") {
    val l = MutableLinkedList(1,2,3)
    assert(!l.exists(5))
  }

  test("head") {
    val l = MutableLinkedList(1,2,3)
    assert(l.head == 1)
  }

  test("empty head") {
    val l = MutableLinkedList()
    assertThrows[NoSuchElementException] {
      l.head
    }
  }

  test("tail") {
    val l = MutableLinkedList(1,2,3)
    assert(l.tail.toString == "2,3")
  }

  test("one MutableList tail") {
    val l = MutableLinkedList(1)
    assertThrows[NoSuchElementException] {
      l.tail
    }
  }

  test("delete") {
    val l = MutableLinkedList(1,2,3,4)
    l.delete(3)
    assert(l.toString == "1,2,4")
  }

  test("delete first") {
    val l = MutableLinkedList(1,2,3,4)
    l.delete(1)
    assert(l.toString == "2,3,4")
  }

  test("delete invalid") {
    val l = MutableLinkedList(1,2,3,4)
    assertThrows[NoSuchElementException] {
      l.delete(5)
    }
  }

}
