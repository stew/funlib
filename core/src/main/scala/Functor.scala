package fun

trait Functor[F[_]] {
  self =>

  import Functor._

  def map[A,B](fa: F[A])(f: A=>B): F[B]

  def compose[G[_]](implicit G: Functor[G]) = new CompositionFunctor[F,G]()(this,implicitly)


}

class CompositionFunctor[F[_]:Functor,G[_]:Functor] extends Functor[({type FG[A] = F[G[A]]})#FG] {
  override def map[A,B](fga: F[G[A]])(f: A=>B): F[G[B]] = Functor[F].map(fga)(Functor[G].map(_)(f))
}

object Functor {
  def apply[F[_]](implicit ev: Functor[F]) = ev
}

