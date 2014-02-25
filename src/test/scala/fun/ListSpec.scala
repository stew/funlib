package fun
package test

import scala.{List ⇒ SList}
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import Arbitrary._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.AllInstances._

object ListSpec extends Properties("List") {

  implicit def pfGen[A,B](implicit aa: Arbitrary[A], ab: Arbitrary[B]) = for {
    f ← arbitrary[A ⇒ B]
    b ← arbitrary[A ⇒ Boolean]
  } yield new PartialFunction[A,B] {
    override def isDefinedAt(a: A): Boolean = b(a)
    override def apply(a: A): B = f(a)
  }
  implicit def arbitraryPF[A,B](implicit aa: Arbitrary[A], ab: Arbitrary[B]): Arbitrary[PartialFunction[A,B]] = Arbitrary(pfGen[A,B])

  property("isEmpty") = forAll((a: SList[String]) => a.isEmpty == List.fromList(a).isEmpty)
  property("nonEmpty") = forAll((a: SList[String]) => a.nonEmpty == List.fromList(a).nonEmpty)
  property("headMaybe") = forAll((a: SList[String]) => Maybe.fromOption(a.headOption) == List.fromList(a).headMaybe)

  property("sum with foldl") = forAll((a: SList[Int]) ⇒ a.sum == List.fromList(a).foldl[Int]((_ + _), 0))
  property("sum with foldr") = forAll((a: SList[Int]) ⇒ a.sum == List.fromList(a).foldr[Int]((_ + _), 0))

  property("collect") = forAll((a: SList[String], pf: PartialFunction[String,Int]) ⇒ a.collect(pf) == List.fromList(a).collect(pf).toList)

  property("filter") = forAll((a: SList[String], f: String⇒Boolean) ⇒ a.filter(f) == List.fromList(a).filter(f).toList)
  property("append") = forAll((a: SList[String], b: SList[String]) ⇒ a ++ b == (List.fromList(a) ++ List.fromList(b)).toList)
}

class ListLawsSpec extends Spec {

  import fun.List._
  import scalaz.Equal
  import scalaz.std.anyVal._
  import scalaz.{ApplicativePlus,Traverse}

  checkAll(monoid.laws[List[Int]])
  checkAll(monadPlus.laws[List])
//  checkAll(traverse.laws[scala.collection.immutable.List])
}
