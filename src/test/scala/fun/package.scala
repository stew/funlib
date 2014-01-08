package fun

import scala.{List⇒SList}
import org.scalacheck.Arbitrary
import Arbitrary._

package object test {

  implicit def arbitraryList[A: Arbitrary]: Arbitrary[List[A]] = Arbitrary {
    arbitrary[SList[A]] map List.fromList
  }

  import scalaz.Order
  implicit def arbitraryLeftHeap[A: Arbitrary: Order]: Arbitrary[LeftHeap[A]] = Arbitrary {
    arbitrary[List[A]].map(_.foldl[LeftHeap[A]]((h,i) ⇒ h.insert(i), LeftHeap.empty))
  }
}
