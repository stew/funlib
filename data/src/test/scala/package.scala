package fun

import scala.{List⇒SList}
import org.scalacheck.Arbitrary
import org.scalacheck.util.Buildable
import Arbitrary._

package object test {

  implicit def listTraversable[A]: List[A] => Traversable[A] = list => {
    list.foldr[SList[A]](_ :: _, Nil)
  }

  implicit def listBuildable[A] = new Buildable[A,List[A]] {
    def builder = new NastyListAppend[A]
  }

  def x[A: Arbitrary]: Arbitrary[List[A]] = Arbitrary.arbContainer(implicitly, implicitly, implicitly)


  implicit def arbitraryLeftHeap[A: Arbitrary: Order]: Arbitrary[LeftHeap[A]] = Arbitrary {
    arbitrary[List[A]].map(_.foldl[LeftHeap[A]]((h,i) ⇒ h.insert(i), LeftHeap.empty))

  }
}
