package immutable

import org.scalatest.FunSuite

class TestImmutableLinkedList extends FunSuite {

  test("apply") {
    val l = ImmutableLinkedList(1,2,3)
    assert(l.toString == "1,2,3")
  }

  test("empty apply") {
    val l = ImmutableLinkedList()
    assert(l == EmptyList)
    assert(l.toString == "")
  }

  test("one element toString") {
    val l = ImmutableLinkedList(1)
    assert(l.toString == "1")
  }

  test("cons") {
    val l = ImmutableLinkedList(1,2,3)
    val m = 4 :: l
    assert(m.toString == "4,1,2,3")
  }

  test("empty list cons") {
    val l = ImmutableLinkedList()
    val m = 4 :: l
    assert(m.toString == "4")
  }

  test("list cons") {
    val l1 = ImmutableLinkedList(1,2,3)
    val l2 = ImmutableLinkedList(4,5,6)
    val m = l1 ::: l2
    assert(m.toString == "1,2,3,4,5,6")
  }

  test("empty second list cons") {
    val l1 = ImmutableLinkedList(1,2,3)
    val l2 = ImmutableLinkedList()
    val m = l1 ::: l2
    assert(m.toString == "1,2,3")
  }

  test("empty first list cons") {
    val l1 = ImmutableLinkedList()
    val l2 = ImmutableLinkedList(4,5,6)
    val m = l1 ::: l2
    assert(m.toString == "4,5,6")
  }

  test("both empty list cons") {
    val l1 = ImmutableLinkedList()
    val l2 = ImmutableLinkedList()
    val m = l1 ::: l2
    assert(m == EmptyList)
  }

  test("covariance cons") {
    val l = ImmutableLinkedList(1,2,3)
    val m = "abc" :: l
    assert(m.toString == "abc,1,2,3")
  }

  test("covariance list cons") {
    val l1 = ImmutableLinkedList(1,2,3)
    val l2 = ImmutableLinkedList("abc", "def")
    val m = l1 ::: l2
    assert(m.toString == "1,2,3,abc,def")
  }

  test("fold left") {
    val l1 = ImmutableLinkedList(1,2,3,4)
    val sum = l1.foldLeft(0)(_ + _)
    assert(sum == 10)
    val product = l1.foldLeft(1)(_ * _)
    assert(product == 24)
  }

  test("empty fold left") {
    val l = ImmutableLinkedList[Int]()
    val sum = l.foldLeft(5)(_ + _)
    assert(sum == 5)
  }

  test("fold right") {
    val l = ImmutableLinkedList(8,4,2)
    val divide = l.foldRight(1.0)(_ / _)
    assert(divide == 4)
  }

  test("empty fold right") {
    val l = ImmutableLinkedList[Int]()
    val divide = l.foldRight(5)(_ / _)
    assert(divide == 5)
  }

  test("reverse") {
    val l = ImmutableLinkedList(1,2,3,4)
    val m = l.reverse()
    assert(m.toString == "4,3,2,1")
  }

  test("empty reverse") {
    val l = ImmutableLinkedList()
    val m = l.reverse()
    assert(m == EmptyList)
  }

  test("reverse single node") {
    val l = ImmutableLinkedList(1)
    val m = l.reverse()
    assert(m.toString == "1")
  }

  test("test map") {
    val l = ImmutableLinkedList(1,2,3)
    val m = l.map[Int, Int](x => x * x)
    assert(m.toString == "1,4,9")
  }

  test("empty map") {
    val l = ImmutableLinkedList[Int]()
    val m = l.map[Int, Int](x => x * x)
    assert(m == EmptyList)
  }
}
