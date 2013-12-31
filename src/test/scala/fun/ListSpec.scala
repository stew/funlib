package fun
package test

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import Arbitrary._
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.AllInstances._

object ListSpec extends Properties("Lst") {

  implicit def pfGen[A,B](implicit aa: Arbitrary[A], ab: Arbitrary[B]) = for {
    f ← arbitrary[A ⇒ B]
    b ← arbitrary[A ⇒ Boolean]
  } yield new PartialFunction[A,B] {
    override def isDefinedAt(a: A): Boolean = b(a)
    override def apply(a: A): B = f(a)
  }
  implicit def arbitraryPF[A,B](implicit aa: Arbitrary[A], ab: Arbitrary[B]): Arbitrary[PartialFunction[A,B]] = Arbitrary(pfGen[A,B])

  property("isEmpty") = forAll((a: List[String]) => a.isEmpty == Lst.fromList(a).isEmpty)
  property("nonEmpty") = forAll((a: List[String]) => a.nonEmpty == Lst.fromList(a).nonEmpty)
  property("headOption") = forAll((a: List[String]) => Maybe.fromOption(a.headOption) == Lst.fromList(a).headOption)

  property("sum with foldl") = forAll((a: List[Int]) ⇒ a.sum == Lst.fromList(a).foldl[Int]((_ + _), 0))
  property("sum with foldr") = forAll((a: List[Int]) ⇒ a.sum == Lst.fromList(a).foldr[Int]((_ + _), 0))

  property("collect") = forAll((a: List[String], pf: PartialFunction[String,Int]) ⇒ a.collect(pf) == Lst.fromList(a).collect(pf).toList)

  property("filter") = forAll((a: List[String], f: String⇒Boolean) ⇒ a.filter(f) == Lst.fromList(a).filter(f).toList)
  property("append") = forAll((a: List[String], b: List[String]) ⇒ a ++ b == (Lst.fromList(a) ++ Lst.fromList(b)).toList)
}

class ListLawsSpec extends Spec {

  import Lst._
  import scalaz.Equal
  import scalaz.std.anyVal._

  checkAll(monoid.laws[Lst[Int]])
  checkAll(monadPlus.laws[Lst])
//  checkAll(cobind.laws[Lst])
  }
