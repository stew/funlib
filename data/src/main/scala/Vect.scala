package fun
import Nat._

/**
  * Exploration of implementing a compiler verified linked list
  * 
  * Otherwise non-total functions, such as head, tail are only
  * available when there is evidence at compile time that the list is
  * non-empty.  Other functions such as "take" and "drop" should
  * verify that there are provably enough elements in the Vect to
  * perform the operation.
  */
trait Vect[A, L <: Nat] {
  def isEmpty: Boolean
  def headMaybe: Maybe[A]
  /**
    * prove that this list is non-empty, and I'll give you a cookie
    */
  def nonEmpty(implicit pred: Pred[L]): VCons[A, pred.Out] = this.asInstanceOf[VCons[A,pred.Out]]


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
  def drop[N <: Nat](dropEv: Drop[A,L,N]): Vect[A,dropEv.Out] = dropEv(this)

  /**
    * Make a Vect using the first N terms of this Vect. If it cannot
    * be verified that there are at least N elements in this Vect,
    * this should fail to compile
    */
  def take[N <: Nat](implicit takeEv: this.type ⇒ Take[A, N]): Vect[A,N] = takeEv(this).apply

  /**
    * split a Vect at position N, returning two Vects. This should be the same as (take[N],drop[N])
    */
  def splitAt[N <: Nat](implicit splitAtEv: SplitAt[A,L,N]): (Vect[A,N], Vect[A,splitAtEv.Out]) = splitAtEv(this)
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
abstract class SplitAt[A, L <: Nat, N <: Nat] {
  type Out <: Nat
  def apply(in: Vect[A, L]): (Vect[A,N], Vect[A,Out])
}

abstract class SplitAtAux[A, L <: Nat, N <: Nat, M <: Nat] {
  def apply(in: Vect[A, L]): (Vect[A,N], Vect[A,M])
}

object SplitAt {
  implicit def fromAux[A, L <: Nat, N <: Nat, M <: Nat](implicit aux: SplitAtAux[A, L, N, M]): SplitAt[A, L, N] = new SplitAt[A, L, N] {
    type Out = M
    def apply(in: Vect[A, L]): (Vect[A,N], Vect[A,Out]) = aux(in)
  }
}

object SplitAtAux {
  /**
    * Provide evidence that any Vect can be split at Zero
    */
  implicit def splitAt0[A, L <: Nat]: SplitAtAux[A, L, _0, L] = new SplitAtAux[A, L, _0, L] {
    override def apply(in: Vect[A, L]) = (VEnd(), in)
  }

  /**
    * Provide evidence that if a Vect[A,L] can be split at N, then we
    * can also split a Vect with an additional element Consed on the
    * front at Succ[N]
    */
  implicit def splitAtN[A, L <: Nat, N <: Nat, M <: Nat](implicit splitAtEv: SplitAtAux[A,L,N,M]): SplitAtAux[A, Succ[L], Succ[N], M] = 
    new SplitAtAux[A, Succ[L], Succ[N], M] { 
      override def apply(in: Vect[A, Succ[L]]) = {
        val prev = splitAtEv(in.tail)
        (in.head :: prev._1, prev._2)
      }
    }
}

/**
  * Drop N items from a Vect leaving M items
  */
trait Drop[A, L <: Nat, N <: Nat] {
  type Out <: Nat
  def apply(in: Vect[A, L]): Vect[A,Out]
}

trait DropAux[A, L <: Nat, N <: Nat, M <: Nat] { 
  def apply(in: Vect[A, L]): Vect[A,M]
}

object Drop {
  implicit def dropaux[A, L <: Nat, N <: Nat, M <: Nat](implicit aux: DropAux[A,L,N,M]): Drop[A,L,N] = new Drop[A,L,N] {
    type Out = M
    def apply(in: Vect[A, L]): Vect[A,Out] = aux.apply(in)
  }
}

object DropAux {
  /**
    * Provide evidence that we can Always drop zero items from a Vect
    */
  implicit def drop0[A, L <: Nat]: DropAux[A, L, _0, L] = new DropAux[A, L, _0, L] {
    def apply(in: Vect[A, L]) = in
  }

  /**
    * Provide evidence that if we can drop L items from the tail, we can drop Succ[L] items from a Vect
    */
  implicit def dropN[A, L <: Nat, N <: Nat, M <: Nat](dropEv: DropAux[A,L,N,M]): Vect[A, Succ[L]] ⇒ DropAux[A, Succ[L], Succ[N], M] = { vect ⇒
    new DropAux[A, Succ[L], Succ[N], M] {
      def apply(in: Vect[A, Succ[L]]) = dropEv(vect.tail)
    }
  }
}

/**
  * A cons cell for a non empty Vect which has a tail of length L
  */
final case class VCons[A, L <: Nat](head: A, tail: Vect[A, L]) extends Vect[A, Succ[L]] {
  def isEmpty = false
  def headMaybe = There(head)
}

/**
  * the end of all Vects
  */
case object VEnd extends Vect[Nothing, _0] {
  def isEmpty = true
  val headMaybe = NotThere

  def unapply[A,N <: Nat](l: Vect[A,N]) = l.isEmpty
  def apply[A]() = this.asInstanceOf[Vect[A,_0]]
}
