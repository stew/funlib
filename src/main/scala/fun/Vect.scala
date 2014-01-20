package fun

trait Vect[A, L <: Nat] {
  vect ⇒

  def isEmpty: Boolean
  def nonEmpty: Boolean
  def headMaybe: Maybe[A]

  /**
    * Drop N items from a Vect, leaving M items
    */
  abstract class Drop[N <: Nat](implicit val diff: Diff[L,N]) {
    def apply: Vect[A,diff.Out]
  }

  /**
    * drop N items from a vector, leaving M Items
    */
  // note, this is not yet working, not sure why
  def drop[N <: Nat](implicit dropEv: Drop[N]) = dropEv.apply

}

object Vect {

  /**
    * Provide evidence that we can Always drop zero items from a Vect
    */
  // note, this is not yet working, not sure why
  implicit def drop0[A,L <: Nat](vect: Vect[A,L])(implicit diff: Diff[L,Z.type]) = new vect.Drop[Z.type]()(diff) {
    def apply = vect.asInstanceOf[Vect[A, this.diff.Out]]
  }

}

case class VCons[A, L <: Nat](head: A, tail: Vect[A, L]) extends Vect[A,Succ[L]] {
  def isEmpty = false
  def nonEmpty = true

  def headMaybe = There(head)

  // note, this is not yet working, not sure why
  implicit def dropN[N <: Nat](implicit dropEv: tail.Drop[N], diff: Diff[L,N]): Drop[Succ[N]] = {
    implicit val diffaux: DiffAux[L,N,diff.Out] = new DiffAux[L,N,diff.Out] {} 
    new Drop[Succ[N]]() {
      def apply = dropEv.apply.asInstanceOf[Vect[A,this.diff.Out]]
    }
  }
}


case object VEnd extends Vect[Nothing, Z.type] {
  def isEmpty = true
  def nonEmpty = false

  val headMaybe = NotThere

  def unapply[A,N <: Nat](l: Vect[A,N]) = l.isEmpty
  def apply[A]() = this.asInstanceOf[Vect[A,Z.type]] // YOLO
}
