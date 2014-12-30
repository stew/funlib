package fun

sealed abstract class Free[F[_]: Functor, A] {
  import Free._
  import Sum._

  def map[B](f: A=>B): Free[F, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Free[F,B]): Free[F,B] = this match {
    case a @ Bind() => bind[F,a.C,B](a.fc,(x => bind(() => a.bind(x),f)))
    case a => bind(() => a,f)
  }

/*
  /** Runs to completion, using a function that extracts the resumption
    * from its suspension functor. */
  final def go(f: F[Free[F, A]] => Free[F, A]): A = {
    @annotation.tailrec
    def go2(t: Free[F, A]): A =
      Tag.unwrap(t.step) match {
        case Left(x) => go2(f(x))
        case Right(x) => x
      }
    go2(this)
  }
 */

  final def step: F[Free[F,A]] \/ A = this match {
    case Pure(a) => Win(a)
    case Suspend(a) => Lose(a)
    case b @ Bind() => b.fc() match {
      case Pure(a) => b.bind(a).step
      case Suspend(a) => Lose(Functor[F].map(a)(_ flatMap b.bind))
      case c @ Bind() => c.fc().flatMap(c.bind(_) flatMap b.bind).step
    }
  }

  /** Interpret a free monad over a free functor of `F` via natural transformation to monad `M`. */
  def run[M[_] : Monad](interp: F ~> M): M[A] = {
    def run0(fa: Free[F,A]): M[A] = fa match {
      case Pure(a) => Monad[M].point(a)
      case Suspend(fa) => Monad[M].flatMap(interp(fa))(run0)
      case b @ Bind() => Monad[M].flatMap(b.fc().run(interp))(a => run0(b.bind(a)))
    }
    run0(this)
  }
}

object Free {
  case class Pure[F[_] : Functor,A](a: A) extends Free[F, A]
  case class Suspend[F[_] : Functor,A](a: F[Free[F,A]]) extends Free[F, A]

  type Trampoline[A] = Free[Function0, A]

  private sealed abstract case class Bind[F[_] : Functor,A]() extends Free[F,A] {
    type C
    val fc: () => Free[F,C]
    val bind: C => Free[F,A]
  }

  def point[F[_] : Functor,A](a: => A): Free[F,A] = Pure(a)


  def bind[F[_] : Functor,A,B](fa: () => Free[F,A], f: A => Free[F,B]): Free[F,B] = new Bind[F,B] {
    type C = A
    val fc = fa
    val bind = f
  }

  implicit def freeMonad[F[_]: Functor]: Monad[({type λ[α]=Free[F, α]})#λ] =
    new Monad[({type λ[α]=Free[F, α]})#λ] {

      override def point[A](a: => A): Free[F,A] = Pure(a)

      override def flatMap[A,B](fa: Free[F,A])(f: A=>Free[F,B]): Free[F,B] = fa match {
        case b @ Bind() => bind(() => b.fc(), ((c:b.C) => bind(() => b.bind(c), f)))
        case fa => bind(() => fa, f)
      }
    }

/* // if we didn't have Bind, but this would not be stack safe
      override def flatMap[A,B](fa: Free[F,A])(f: A => Free[F,B]): Free[F,B] = fa match {
        case Pure(a) => f(a)
        case Suspend(a) => Suspend(Functor[F].map(a)(flatMap(_)(f)))
      }
o    }
 */

  def liftF[F[_]: Functor, A](value: => F[A]): Free[F,A] =
    Suspend(Functor[F].map(value)(Pure(_)))

  type FreeC[F[_],A] = Free[({type λ[α]=Coyoneda[F,α]})#λ, A]

  def liftFC[F[_], A](value: => Coyoneda[F,A]): FreeC[F,A] = liftF[({type λ[α]=Coyoneda[F,α]})#λ, A](value)
}
