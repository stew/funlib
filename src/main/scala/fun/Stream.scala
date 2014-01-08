package fun

import scalaz.Equal

sealed trait Stream[A] {
  def headMaybe: Maybe[A]
  def tail: Stream[A]

  def foldLeft[B](b: B)(f: (B,A)⇒B): B = {
    headMaybe match {
      case There(a) ⇒ tail.foldLeft(f(b,a))(f)
      case _ ⇒ b
    }
  }
}

abstract class StreamCons[A](head: A) extends Stream[A] {
  def headMaybe = There(head)
}

object StreamCons {
  def unapply[A](sa: Stream[A]) = sa.headMaybe.toOption
}

case object StreamNil extends Stream[Nothing] {
  def apply[A]() = this.asInstanceOf[Stream[A]]
  def unapply[A](s: Stream[A]): Boolean = (s.headMaybe == NotThere)

  def tail = this // h8

  val headMaybe = NotThere
}

object Stream {
  def apply[A](as: A*): Stream[A] = {
    as.headOption match {
      case None ⇒ StreamNil()
      case Some(x) ⇒ new StreamCons(x) {
        def tail = apply(as.tail : _*)
      }
    }
  }

  def cons[A](h: A, t: ⇒ Stream[A]): Stream[A] = new StreamCons(h) {
    def tail = t
  }

  def empty[A]: Stream[A] = StreamNil()

  def unfold[A, B](seed: A)(f: A => Maybe[(B, A)]): Stream[B] =
    f(seed) match {
      case NotThere() ⇒ Stream.empty
      case There((b, a)) ⇒ Stream.cons(b, unfold(a)(f))
    }

  implicit def streemEqual[A](implicit A0: Equal[A]): Equal[Stream[A]] = new StreamEqual[A] {
    implicit def A = A0
  }

}

private trait StreamEqual[A] extends Equal[Stream[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Stream[A], a2: Stream[A]) = (a1,a2) match {
    case (StreamNil(),StreamNil()) ⇒ true
    case (StreamCons(x), StreamCons(y)) ⇒ A.equal(x,y) && equal(a1.tail, a2.tail)
    case (_, _) ⇒ false
  }

}
