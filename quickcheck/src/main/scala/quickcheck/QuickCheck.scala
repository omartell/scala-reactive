package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("minimum of two insertions") = forAll { (a: Int, b: Int) =>
    val ah  = insert(a, empty)
    val abh = insert(b, ah)

    if(a < b)
      findMin(abh) == a
    else
      findMin(abh) == b
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("empty heap after deleting inserted element") = forAll { (a: Int) =>
    val ah  = insert(a, empty)
    val dh  = deleteMin(ah)

    isEmpty(dh)
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("melding two heaps") = forAll{ (a: Int, b: Int) =>
    val ah = insert(a, empty)
    val bh = insert(b, empty)

    val melded = meld(ah, bh)

    if(a < b)
      findMin(melded) == a
    else
      findMin(melded) == b
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("get a sorted sequence when continually finding and deleting minima") = forAll{ (numbers: List[Int]) =>
    val h = numbers.foldLeft(empty)((h: H, n: A) => insert(n, h))
    findSortedSequence(h, List()) == numbers.sortBy(n => n)
  }

  def findSortedSequence(h: H, seq: List[Int]): List[Int] = {
    if(isEmpty(h))
      seq
    else
      findSortedSequence(deleteMin(h), seq++List(findMin(h)))
  }





  lazy val genHeap: Gen[H] = for{
    k <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
