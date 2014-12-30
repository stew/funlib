package fun

trait Equal[A] {
  def equal(left: A, right: A): Boolean
}

object Equal {
  def apply[A: Equal]: Equal[A] = implicitly
}
