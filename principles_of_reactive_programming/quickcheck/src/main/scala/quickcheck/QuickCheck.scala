package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == min(a, b)
  }
  
  property("min_meld") = forAll { (h1: H, h2: H) =>
    def check(min: Int) = {
      if (isEmpty(h1)) findMin(h2) == min
      else if (isEmpty(h2)) findMin(h1) == min
      else findMin(h1) == min || findMin(h2) == min
    }
    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val min = findMin(meld(h1, h2)) 
      check(min)
    }
  }
  
  
  
  property("delete1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("delete2") = forAll { a: Int =>
    def deleteCheck(h: H, x: Int) = {
      findMin(deleteMin(h)) == x
    }
    if (a > 0) {
      val h = insert(0, insert(a, empty))
      deleteCheck(h, a)
    } else if (a < 0) {
      val h = insert(a, insert(0, empty))
      deleteCheck(h, 0)
    } else {
      val h = insert(-1, insert(0, empty))
      deleteCheck(h, 0)
    }
 }
  
    property("delete3") = forAll { a: Int => 
      if (a > 0)
        findMin(deleteMin(insert(0, insert(-1, insert(a, empty))))) == 0
      else
        findMin(deleteMin(insert(1, insert(0, insert(a, empty))))) == 0

    }  
    
  property("get_sorted") = forAll { h: H =>
    def get(heap: H, xs: List[Int]): List[Int] = {
      if (isEmpty(heap)) xs.reverse
      else {
        val x = findMin(heap)
        get(deleteMin(heap), x::xs)
      }
    }
    val list = get(h, Nil)
    list == list.sorted
  }
  
  lazy val genEmpty = value(empty)
  
  lazy val genInsert: Gen[H] = for {
    v <- arbitrary[Int] 
    h <- genHeap
  } yield insert(v, h)

  lazy val genMeld = for {
    h1 <- lzy(genHeap)
    h2 <- genHeap
  } yield meld(h1, h2)
  
  lazy val genDeleteMin = for {
    h <- lzy(genHeap)
    if !isEmpty(h)
  } yield deleteMin(h)
  
  lazy val genHeap: Gen[H] =  
    //oneOf(genInsert, genMeld, genDeleteMin, genEmpty) 
    Gen.frequency((15, genInsert), (3, genDeleteMin), (3, genMeld), (5, genEmpty)) 
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
