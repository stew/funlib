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
import scalaz.{Order,Show}
import scalaz.Ordering._

object LeftHeapSpec extends Properties("LeftHeap") {
  import LeftHeap._

  def ordered[A](h: LeftHeap[A])(implicit order: Order[A], showa: Show[A]): Boolean = {

    val combine: ((Boolean, Maybe[A]),  A) ⇒ (Boolean,Maybe[A]) = {
      case ((true,NotThere), i) ⇒ (true, There(i))
      case ((b, There(x)), i) ⇒ (b && order(x,i) != GT, There(i))
    }

    val r = h.toStream.foldLeft[(Boolean, Maybe[A])]((true, NotThere()))(combine)
    r._1
  }


  property("ordered") = forAll { (as: Lst[String]) ⇒ 
    val h = as.foldl[LeftHeap[String]]({(r, a) ⇒ r insert a}, LeftHeap.empty[String])
    ordered(h)
  }
}

class LeftHeapLawsSpec extends Spec {

  import LeftHeap._
  import scalaz.Equal
  import scalaz.std.anyVal._

  checkAll(monoid.laws[LeftHeap[Int]])
}
