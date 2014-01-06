package fun

import scalaz.Equal

sealed trait Streem[A] {
  def headMaybe: Maybe[A]
  def tail: Streem[A]

  def foldLeft[B](b: B)(f: (B,A)⇒B): B = {
    headMaybe match {
      case There(a) ⇒ tail.foldLeft(f(b,a))(f)
      case _ ⇒ b
    }
  }
}

abstract class StreemCons[A](head: A) extends Streem[A] {
  def headMaybe = There(head)
}

object StreemCons {
  def unapply[A](sa: Streem[A]) = sa.headMaybe.toOption
}

case object StreemNil extends Streem[Nothing] {
  def apply[A]() = this.asInstanceOf[Streem[A]]
  def unapply[A](s: Streem[A]): Boolean = (s.headMaybe == NotThere)

  def tail = this // h8

  val headMaybe = NotThere
}

object Streem {
  def apply[A](as: A*): Streem[A] = {
    as.headOption match {
      case None ⇒ StreemNil()
      case Some(x) ⇒ new StreemCons(x) {
        def tail = apply(as.tail : _*)
      }
    }
  }

  def cons[A](h: A, t: ⇒ Streem[A]): Streem[A] = new StreemCons(h) {
    def tail = t
  }

  def empty[A]: Streem[A] = StreemNil()

  def unfold[A, B](seed: A)(f: A => Maybe[(B, A)]): Streem[B] =
    f(seed) match {
      case NotThere() ⇒ Streem.empty
      case There((b, a)) ⇒ Streem.cons(b, unfold(a)(f))
    }

  implicit def streemEqual[A](implicit A0: Equal[A]): Equal[Streem[A]] = new StreemEqual[A] {
    implicit def A = A0
  }

}

private trait StreemEqual[A] extends Equal[Streem[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Streem[A], a2: Streem[A]) = (a1,a2) match {
    case (StreemNil(),StreemNil()) ⇒ true
    case (StreemCons(x), StreemCons(y)) ⇒ A.equal(x,y) && equal(a1.tail, a2.tail)
    case (_, _) ⇒ false
  }

}
