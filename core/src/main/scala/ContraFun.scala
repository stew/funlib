package fun

trait ContraFunctor[F[_]] {
  def contramap[A,B](fa: F[A])(f: B=>A): F[B]
}
