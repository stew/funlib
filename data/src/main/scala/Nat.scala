package fun

sealed trait Nat

final case class Succ[N <: Nat](p: Nat) extends Nat

object Nat {
  class _0 extends Nat
  implicit val _0 = new _0
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
