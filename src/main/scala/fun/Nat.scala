package fun

sealed trait Nat

final case object Z extends Nat
final case class Succ[N <: Nat](p: Nat) extends Nat
