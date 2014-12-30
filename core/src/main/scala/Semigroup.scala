package fun

trait Semigroup[A] {
  def zero: A
}

object Semigroup {
  def apply[A: Semigroup]: Semigroup[A] = implicitly
}
