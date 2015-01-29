package fun

sealed trait Trampoline[A] {
  final def run: A = this match {
    case Return(a) => a
    case Suspend(cont) => cont().run
    case FlatMap(fa, cont) => cont(fa.run).run
  }

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = FlatMap(this, f)
  def map[B](f: A => B) = this.flatMap[B](f andThen (Return.apply _))
}

case class Suspend[A](cont: () => Trampoline[A]) extends Trampoline[A]
case class Return[A](value: A) extends Trampoline[A]
case class FlatMap[A,B](from: Trampoline[A], to: A => Trampoline[B]) extends Trampoline[B]
