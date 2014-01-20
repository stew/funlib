package fun

trait Vect[A, L <: Nat] {
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def headMaybe: Maybe[A]
  /**
    * drop N items from a vector, leaving M Items
    */
  // note, this is not yet working, not sure why
  import Drop._
  def drop[N <: Nat](implicit dropEv: this.type ⇒ Drop[A,L,N], diff: Diff[L,N]) = dropEv(this).apply

  def tail(implicit pred: Pred[L]): Vect[A,pred.Out] = this.asInstanceOf[VCons[A,pred.Out]].tail

}


/**
  * Drop N items from a Vect, leaving M items
  */
abstract class Drop[A, L <: Nat, N <: Nat](implicit val diff: Diff[L,N]) {
  def apply: Vect[A,diff.Out]
}

object Drop {
  /**
    * Provide evidence that we can Always drop zero items from a Vect
    */
  // note, this is not yet working, not sure why
  implicit def drop0[A, L <: Nat](implicit diff: Diff[L,Z.type]): Vect[A,L] ⇒ Drop[A, L, Z.type] = { vect ⇒ 
    new Drop[A,L,Z.type]()(diff) {
      def apply = vect.asInstanceOf[Vect[A, this.diff.Out]]
    }
  }
  implicit def dropN[A, L <: Nat, N <: Nat](implicit dropEv: Vect[A,L] ⇒ Drop[A,L,N], diff: Diff[L,N]): Vect[A, Succ[L]] ⇒ Drop[A, Succ[L], Succ[N]] = { vect ⇒
    implicit val pdiff: DiffAux[L,N,diff.Out] = new DiffAux[L,N,diff.Out] {}
    new Drop[A, Succ[L], Succ[N]]() {
      def apply = dropEv(vect.tail).apply.asInstanceOf[Vect[A,this.diff.Out]]
    }
  }
}

case class VCons[A, L <: Nat](head: A, tail: Vect[A, L]) extends Vect[A,Succ[L]] {

  def isEmpty = false
  def nonEmpty = true

  def headMaybe = There(head)

}


case object VEnd extends Vect[Nothing, Z.type] {
  def isEmpty = true
  def nonEmpty = false

  val headMaybe = NotThere

  def unapply[A,N <: Nat](l: Vect[A,N]) = l.isEmpty
  def apply[A]() = this.asInstanceOf[Vect[A,Z.type]] // YOLO
}
