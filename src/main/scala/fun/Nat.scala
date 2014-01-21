package fun

sealed trait Nat

final case object Z extends Nat
final case class Succ[N <: Nat](p: Nat) extends Nat
trait Plus[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait Diff[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait DiffAux[A <: Nat, B <: Nat, C <: Nat]

object Diff {
  implicit def diff[A <: Nat, B <: Nat, C <: Nat](implicit diffAux: DiffAux[A,B,C]) = new Diff[A,B] {
    type Out = C
  }
}

object DiffAux {
  implicit def diffAux0[A <: Nat] = new DiffAux[A, Z.type, A] {}
  implicit def diffAuxN[A <: Nat, B <: Nat, C <: Nat](implicit ev: DiffAux[A,B,C]) = 
    new DiffAux[Succ[A], Succ[B], C] {}

}


trait Pred[N <: Nat] {
  type Out <: Nat
}

trait PredAux[N <: Nat, P <: Nat]

object Pred {
  implicit def pred[N <: Nat, P <: Nat](implicit ev : PredAux[N, P]) = new Pred[N] {
    type Out = P
  }
}

object PredAux {
  implicit def pred[P <: Nat] = new PredAux[Succ[P], P] {} 
}
