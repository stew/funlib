package fun

import org.scalacheck.Arbitrary
import Arbitrary._

package object test {

  implicit def arbitraryList[A: Arbitrary]: Arbitrary[Lst[A]] = Arbitrary {
    arbitrary[List[A]] map Lst.fromList
  }

  import scalaz.Order
  implicit def arbitraryLeftHeap[A: Arbitrary: Order]: Arbitrary[LeftHeap[A]] = Arbitrary {
    arbitrary[List[A]].map(_.foldLeft[LeftHeap[A]](LeftHeap.empty)((h,i) ⇒ h.insert(i)))
  }
}
