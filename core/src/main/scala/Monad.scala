package fun

trait Monad[F[_]] extends Applicative[F] {
  override def map[A,B](fa: F[A])(f: A=>B): F[B] =
    flatMap(fa)((x:A) => point(f(x)))

  override def ap[A,B](fa: F[A])(ff: F[A => B]): F[B] = 
    flatMap(fa)(a => map(ff)(f => f(a)))

  def flatMap[A,B](fa: F[A])(f: A=>F[B]): F[B]
}

object Monad {
  def apply[F[_] : Monad]: Monad[F] = implicitly
}
