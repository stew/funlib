package fun

trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly
}
