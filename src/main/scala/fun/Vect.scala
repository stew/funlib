package fun
import Nat._0

/**
  * Exploration of implementing a compiler verified linked list
  * 
  * Otherwise non-total functions, such as head, tail are only
  * available when there is evidence at compile time that the list is
  * non-empty.  Other functions such as "take" and "drop" should
  * verify that there are provably enough elements in the Vect to
  * perform the operation.
  */
// TODO get rid of the YOLO .asInstanceOf all over the place, probably by adding Leibniz.===
trait Vect[A, L <: Nat] {
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def headMaybe: Maybe[A]

  /**
    *  If we have proof that L is not Zero, we can safely return the tail of the Vect
    */
  def tail(implicit pred: Pred[L]): Vect[A,pred.Out] = this.asInstanceOf[VCons[A,pred.Out]].tail // demi-YOLO?

  /**
    *  If we have proof that L is not Zero, we can safely return the head of the Vect
    */
  def head(implicit pred: Pred[L]): A = this.asInstanceOf[VCons[A,pred.Out]].head 

  /**
    * Prepend an element to the front of this Vect
    */
  def ::(head: A): VCons[A, L] = VCons(head, this)

  /**
    * drop N items from a Vect, leaving M Items. If we cannot find
    * proof that there are at least N elements in this Vect, the call
    * should fail to compile.
    */
  def drop[N <: Nat](implicit dropEv: this.type ⇒ Drop[A,L,N], diff: Diff[L,N]): Vect[A,diff.Out] = dropEv(this).apply.asInstanceOf[Vect[A,diff.Out]]

  /**
    * Make a Vect using the first N terms of this Vect. If it cannot
    * be verified that there are at least N elements in this Vect,
    * this should fail to compile
    */
  def take[N <: Nat](implicit takeEv: this.type ⇒ Take[A, N], diff: Diff[L,N]): Vect[A,N] = takeEv(this).apply

  /**
    * split a Vect at position N, returning two Vects. This should be the same as (take[N],drop[N])
    */
  def splitAt[N <: Nat](implicit splitAtEv: this.type ⇒ SplitAt[A,L,N], diff: Diff[L,N]): (Vect[A,N], Vect[A,diff.Out]) = splitAtEv(this).apply.asInstanceOf[(Vect[A,N], Vect[A, diff.Out])]
}

/**
  * Take N items from a Vect
  */
trait Take[A, N <: Nat] {
  def apply: Vect[A,N]
}

object Take {
  /**
    * Provide evidence that we can always drop zero items from a Vect
    */
  implicit def take0[A,L <: Nat]: Vect[A,L] ⇒ Take[A, _0] = { _ ⇒ 
    new Take[A, _0] {
      def apply = VEnd[A]() 
    }
  }

  /**
    * Provided that we know how to take N from the tail of a list,
    * provide evidence that we can take Succ[N] from this list
    */
  implicit def takeN[A,L <: Nat, N <: Nat](implicit take: Vect[A, L] ⇒ Take[A,N]): Vect[A, Succ[L]] ⇒ Take[A,Succ[N]] = { vect ⇒
    new Take[A,Succ[N]] {
      def apply = VCons(vect.head, take(vect.tail).apply)
    }
  }
}

/**
  Split a Vect into two lists at the Nth element of the Vect
  */
abstract class SplitAt[A, L <: Nat, N <: Nat](implicit val diff: Diff[L,N]) {
  def apply: (Vect[A,N], Vect[A,diff.Out])
}

object SplitAt {
  // TODO: we should be able to get rid of the implicit diff here, but
  // we might need to add a Leibniz.=== to do it
  /**
    * Provide evidence that any Vect can be split at Zero
    */
  implicit def splitAt0[A, L <: Nat](implicit diff: Diff[L,_0]): Vect[A,L] ⇒ SplitAt[A, L, _0] = { vect ⇒
    new SplitAt[A, L, _0]()(diff) {
      def apply: (Vect[A,_0], Vect[A,diff.Out]) = (VEnd(), vect.asInstanceOf[Vect[A, diff.Out]])
    }
  }

  /**
    * Provide evidence that if a Vect[A,L] can be split at N, then we
    * can also split a Vect with an additional element Consed on the
    * front at Succ[N]
    */
  implicit def splitAtN[A, L <: Nat, N <: Nat](implicit diff: Diff[L,N], splitAtEv: Vect[A,L] ⇒ SplitAt[A,L,N]): Vect[A, Succ[L]] ⇒ SplitAt[A, Succ[L], Succ[N]] = { vect ⇒
    implicit val pdiff: DiffAux[L,N,diff.Out] = new DiffAux[L,N,diff.Out] {}
    new SplitAt[A, Succ[L], Succ[N]] { 
      def apply = {
        val prev = splitAtEv(vect.tail).apply
        (vect.head :: prev._1, prev._2.asInstanceOf[Vect[A, diff.Out]])
      }
    }
  }
}

/**
  * Drop N items from a Vect of length L
  */
abstract class Drop[A, L <: Nat, N <: Nat, M<: Nat](implicit val diff: Diff[L,N]) {
  def apply: Vect[A,diff.Out]
}

object Drop {
  /**
    * Provide evidence that we can Always drop zero items from a Vect
    */
  implicit def drop0[A, L <: Nat](implicit diff: Diff[L,_0]): Vect[A,L] ⇒ Drop[A, L, _0] = { vect ⇒ 
    new Drop[A,L,_0]()(diff) {
      def apply = vect.asInstanceOf[Vect[A, this.diff.Out]]
    }
  }

  /**
    * Provide evidence that if we can drop L items from the tail, we can drop Succ[L] items from a Vect
    */
  implicit def dropN[A, L <: Nat, N <: Nat](implicit dropEv: Vect[A,L] ⇒ Drop[A,L,N], diff: Diff[L,N]): Vect[A, Succ[L]] ⇒ Drop[A, Succ[L], Succ[N]] = { vect ⇒
    implicit val pdiff: DiffAux[L,N,diff.Out] = new DiffAux[L,N,diff.Out] {}
    new Drop[A, Succ[L], Succ[N]]() {
      def apply = dropEv(vect.tail).apply.asInstanceOf[Vect[A,this.diff.Out]]
    }
  }
}

/**
  * A cons cell for a non empty Vect which has a tail of length L
  */
case class VCons[A, L <: Nat](head: A, tail: Vect[A, L]) extends Vect[A,Succ[L]] {

  def isEmpty = false
  def nonEmpty = true

  def headMaybe = There(head)

}

/**
  * the end of all Vects
  */
case object VEnd extends Vect[Nothing, _0] {
  def isEmpty = true
  def nonEmpty = false

  val headMaybe = NotThere

  def unapply[A,N <: Nat](l: Vect[A,N]) = l.isEmpty
  def apply[A]() = this.asInstanceOf[Vect[A,_0]]
}
