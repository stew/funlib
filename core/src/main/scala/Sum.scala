package fun

trait Sum[+A,+B] {
  import Tag._

  def validation[AA >: A, BB >: B]: Sum[AA,BB] @@ Validation = Tag.of[Validation](this)
  def winLose[AA >: A, BB >: B]: Sum[AA,BB] @@ WinLose = Tag.of[WinLose](this)

  final def map[C](f: B => C): Sum[A,C] = this match {
    case Right(b) => Right(f(b))
    case l @ Left(_) => l
  }

  final def flatMap[AA >: A, C](f: B => Sum[AA,C]): Sum[AA,C] = this match {
    case Right(b) => f(b)
    case l @ Left(_) => l
  }

  final def cata[X](l: A=>X, r: B=>X): X = this match {
    case Left(a) => l(a)
    case Right(b) => r(b)
  }
}

case class Left[+A](a: A) extends Sum[A,Nothing]
case class Right[+B](b: B) extends Sum[Nothing,B]

object Sum extends SumTypes

trait SumTypes extends SumConstructors {
  type \/[A,B] = Sum[A,B] @@ WinLose
  type v[A,B] = Sum[A,B] @@ Validation

  object -\/ {
    def unapply[E](e: Sum[E,Any] @@ WinLose): Option[E \/ Nothing] =
      Tag.unwrap(e) match {
        case Left(e) => Some(Lose(e))
        case _ => None
      }
  }
  object \/- {
    def unapply[A](a: Sum[Any,A] @@ WinLose): Option[Nothing \/ A] =
      Tag.unwrap(a) match {
        case Right(a) => Some(Win(a))
        case _ => None
      }
  }
}

trait SumConstructors {
  def Valid[A,B](b: B): Sum[A,B] @@ Validation = Right(b).validation
  def Invalid[A,B](a: A): Sum[A,B] @@ Validation = Left(a).validation
  def Win[A,B](b: B): Sum[A,B] @@ WinLose = Right(b).winLose
  def Lose[A,B](a: A): Sum[A,B] @@ WinLose = Left(a).winLose
}

trait Validation
trait WinLose

trait WinLoseInstances extends SumConstructors {
  implicit def winLoseMonad[Err] = new Monad[({type λ[α]=Sum[Err,α] @@ WinLose})#λ] {
    override def point[A](a: => A) = Win(a)
    override def flatMap[A,B](fa: Sum[Err,A] @@ WinLose)(f: A=>Sum[Err,B] @@ WinLose):Sum[Err,B] @@ WinLose = 
      (Tag.unwrap(fa).flatMap((x:A) => Tag.unwrap(f(x)))).winLose
  }
}
