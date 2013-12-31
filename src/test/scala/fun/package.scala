package fun

import org.scalacheck.Arbitrary
import Arbitrary._

package object test {

  implicit def arbitraryList[A: Arbitrary]: Arbitrary[Lst[A]] = Arbitrary {
    arbitrary[List[A]] map Lst.fromList
  }

  import scalaz.Order
  implicit def arbitraryHeap[A: Arbitrary: Order]: Arbitrary[Heap[A]] = Arbitrary {
    arbitrary[List[A]].map(_.foldLeft[Heap[A]](Heap.empty)((h,i) ⇒ h.insert(i)))
  }
}
