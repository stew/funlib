package fun
package test

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import Arbitrary._
import std.string._
import Comparison._

object RedBlackTreeSpec extends Properties("RedBlackTree") {
  import RedBlackTree._

  def ordered[A](h: RedBlackTree[A])(implicit order: Order[A]): Boolean = {

    val combine: ((Boolean, Maybe[A]),  A) ⇒ (Boolean,Maybe[A]) = {
      case ((true,NotThere()), i) ⇒ (true, There(i))
      case ((b, There(x)), i) ⇒ (b && order(x,i) != GT, There(i))
    }

    val r = h.toList.foldl[(Boolean, Maybe[A])](combine, (true, NotThere()))
    r._1
  }

  property("ordered") = forAll { (as: List[String]) ⇒ 
    try {
      val h = as.foldl[RedBlackTree[String]]({(r, a) ⇒ r insert a}, RedBlackTree.empty[String])
      ordered(h)
    } catch {
      case e: Throwable ⇒ e.printStackTrace
        false
    }
  }(implicitly, arbContainer(implicitly,implicitly,_.toList), implicitly, implicitly) // this is implicitly sucky, and its because scalacheck wants a Traversable
}

/*
class RedBlackTreeLawsSpec extends Spec {

  import RedBlackTree._

  checkAll(monoid.laws[RedBlackTree[Int]])
}
 */
