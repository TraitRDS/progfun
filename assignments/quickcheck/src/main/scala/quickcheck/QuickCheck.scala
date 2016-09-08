package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val nonEmptyHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), nonEmptyHeap)
  } yield insert(a, h)

  //lazy val genHeap: Gen[H] = oneOf(const(empty), nonEmptyHeap)
  lazy val genHeap: Gen[H] = nonEmptyHeap

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin1") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("deleteMin1") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("findMin and deleteMin1") = forAll{ h: H =>
    var ht = h
    var l = List[A]()
    while (!isEmpty(ht)) {
      l = findMin(ht) :: l
      ht = deleteMin(ht)
    }
    l == l.sorted
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }
}
