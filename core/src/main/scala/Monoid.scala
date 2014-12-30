package fun

trait Monoid[A] extends Semigroup[A] {
  def append(left: A, right: => A): A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly
}
