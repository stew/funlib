package fun

trait Semigroup[A] {
  def append(left: A, right: => A): A
}

object Semigroup {
  def apply[A: Semigroup]: Semigroup[A] = implicitly
}
