package fun
package test

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import Arbitrary._
import Comparison._
import std.string._

object LeftHeapSpec extends Properties("LeftHeap") {
  import LeftHeap._

  def ordered[A](h: LeftHeap[A])(implicit order: Order[A]): Boolean = {

    val combine: ((Boolean, Maybe[A]),  A) ⇒ (Boolean,Maybe[A]) = {
      case ((true,NotThere()), i) ⇒ (true, There(i))
      case ((b, There(x)), i) ⇒ (b && order(x,i) != GT, There(i))
    }

    val r = h.toStream.foldLeft[(Boolean, Maybe[A])]((true, NotThere()))(combine)
    r._1
  }


  property("ordered") = forAll { (as: List[String]) ⇒ 
    val h = as.foldl[LeftHeap[String]]({(r, a) ⇒ r insert a}, LeftHeap.empty[String])
    ordered(h)
  }(implicitly, arbContainer(implicitly,implicitly,_.toList), implicitly, implicitly) // this is implicitly sucky, and its because scalacheck wants a Traversable
}

/*
class LeftHeapLawsSpec extends Spec {

  import LeftHeap._

  checkAll(monoid.laws[LeftHeap[Int]])
}
 */
