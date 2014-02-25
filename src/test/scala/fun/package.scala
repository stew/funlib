package fun

import scala.{List⇒SList}
import org.scalacheck.Arbitrary
import org.scalacheck.util.Buildable
import Arbitrary._

package object test {


  implicit def listBuildable[A] = new Buildable[A,List] {
    def builder = new NastyListAppend[A]
  }

  import scalaz.Order
  implicit def arbitraryLeftHeap[A: Arbitrary: Order]: Arbitrary[LeftHeap[A]] = Arbitrary {
    arbitrary[List[A]].map(_.foldl[LeftHeap[A]]((h,i) ⇒ h.insert(i), LeftHeap.empty))
  }
}
