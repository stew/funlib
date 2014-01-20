package fun

sealed trait Nat

final case object Z extends Nat
final case class Succ[N <: Nat](p: Nat) extends Nat

/**
 * Type class supporting conversion of type-level Nats to value level Ints.
 * 
 * @author Miles Sabin
 */
trait ToInt[N <: Nat] {
  def apply() : Int
}

object ToInt {
  implicit val toInt0 = new ToInt[Z.type] {
    def apply() = 0
  }
  implicit def toIntSucc[N <: Nat](implicit toIntN : ToInt[N]) = new ToInt[Succ[N]] {
    def apply() = toIntN()+1
  }
}

trait Plus[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait PlusAux[A <: Nat, B <: Nat, C <: Nat]

object Plus {
  implicit def plusAux[A <: Nat,B <: Nat, C <: Nat](implicit pa: PlusAux[A,B,C]) = new Plus[A,B] {
    type Out = C
  }
}

object PlusAux {
  implicit def plusAux0[B <: Nat] = new PlusAux[Z.type, B, B] {} 
  implicit def plusAuxN[A <: Nat, B <: Nat, C <: Nat](implicit ev: PlusAux[A,B,C]) = new PlusAux[Succ[A], B, Succ[C]] {} 
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
