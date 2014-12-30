package fun
package test

import Comparison._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import Arbitrary._
import std.string._

object BinomialHeapSpec extends Properties("BinomialHeap") {
  import BinomialHeap._

  def ordered[A](h: BinomialHeap[A])(implicit order: Order[A]): Boolean = {

    val combine: ((Boolean, Maybe[A]),  A) ⇒ (Boolean,Maybe[A]) = {
      case ((true,NotThere()), i) ⇒ (true, There(i))
      case ((b, There(x)), i) ⇒ (b && order(x,i) != GT, There(i))
    }

    val r = h.toStream.foldLeft[(Boolean, Maybe[A])]((true, NotThere()))(combine)
    r._1
  }


  property("ordered") = forAll { (as: List[String]) ⇒ 
    try {
      val h = as.foldl[BinomialHeap[String]]({(r, a) ⇒ r insert a}, BinomialHeap.empty[String])
      ordered(h)
    } catch {
      case e: Throwable ⇒ e.printStackTrace
        false
    }
  }(implicitly, arbContainer(implicitly,implicitly,_.toList), implicitly, implicitly) // this is implicitly sucky, and its because scalacheck wants a Traversable
}
