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

object MaybeSpec extends Properties("Maybe") {
  property("cata") = forAll((a: Option[String], f: String⇒Int, i: Int) ⇒ a.fold(i)(f) == Maybe.fromOption(a).cata(f, i))

  property("filter") = forAll((a: Option[Int], f: Int⇒Boolean) ⇒ (a filter f) == (Maybe.fromOption(a) filter f).toOption)

}

class MaybeLawsSpec extends Spec {
  implicit def arbitraryMaybe[A: Arbitrary] = Arbitrary {
    arbitrary[Option[A]] map Maybe.fromOption
  }

  checkAll(monoid.laws[Maybe[Int]])
  checkAll(monadPlus.laws[Maybe])
}
