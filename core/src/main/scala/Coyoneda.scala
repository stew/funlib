package fun

sealed abstract class Coyoneda[F[_], A] {
  type C
  val fc: F[C]
  val f: C => A
}

object Coyoneda {
  def apply[F[_],A,B](fa: F[A], ff: A=>B = {x:A => x}) = new Coyoneda[F,B] {
    type C = A
    val fc = fa
    val f = ff
  }

  implicit def coyoFunctor[F[_]]: Functor[({type λ[α]=Coyoneda[F, α]})#λ] = new Functor[({type λ[α]=Coyoneda[F, α]})#λ] {
    def map[A,B](fa: Coyoneda[F,A])(ff: A=>B): Coyoneda[F,B] = Coyoneda(fa.fc, fa.f andThen ff)
  }
}
