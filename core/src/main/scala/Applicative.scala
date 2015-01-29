package fun

trait Applicative[F[_]] extends Functor[F] {
  def point[A](a: => A): F[A]

  def ap[A,B](fa: F[A])(ff: F[A => B]): F[B]

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    ap(fb)(map(fa)(f.curried))

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] =
    ap(fc)(ap(fb)(map(fa)(f.curried)))

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] =
    ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried))))

}

object Applicative {
  def apply[F[_]](implicit ev: Applicative[F]) = ev
}
