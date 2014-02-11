import org.scalacheck._
import common._
import Arbitrary._
import Gen._
import Prop._
import quickcheck._

object quickcheckworksheet {
 println("Welcome to the Scala worksheet")        //> Welcome to the Scala worksheet
  
 lazy val genMap: Gen[Map[Int,Int]] = for {
  k <- arbitrary[Int]
  v <- arbitrary[Int]
  m <- oneOf(value(Map.empty[Int,Int]), genMap)
} yield m.updated(k, v)                           //> genMap: => org.scalacheck.Gen[Map[Int,Int]]

val squares = for {
  xs <- Arbitrary.arbitrary[List[Int]]
} yield xs.map(x => x*x)                          //> squares  : org.scalacheck.Gen[List[Int]] = Gen()

genMap.sample                                     //> res0: Option[Map[Int,Int]] = Some(Map(0 -> 2009200960, -105654310 -> 2147483
                                                  //| 647))
genMap.sample                                     //> res1: Option[Map[Int,Int]] = Some(Map(-1032540598 -> 1))
object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap

QuickCheckBinomialHeap.genHeap.sample             //> res2: Option[quickcheckworksheet.QuickCheckBinomialHeap.H] = Some(List(Node(
                                                  //| -1,1,List(Node(1469041250,0,List())))))
QuickCheckBinomialHeap.genHeap.sample             //> res3: Option[quickcheckworksheet.QuickCheckBinomialHeap.H] = Some(List(Node(
                                                  //| -2147483648,2,List(Node(-1947479833,1,List(Node(484436340,0,List()))), Node(
                                                  //| 711745434,0,List())))))
                     
}