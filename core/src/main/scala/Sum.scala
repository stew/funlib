package fun

/**
  * The cross-dressing parent of all sum types. they be right biassed,
  * but we don't hold it against hir.
  * 
  * The intention is that sum types that require specific behaviors
  * via typeclasses will be tagged below.
  * 
  * currently there are two such tags:
  *  valdation aka V, which accumulates many errors and has no Monad
  *  winLose aka \/ which is fail fast, and monadic. first error stops computation
  */
trait Sum[+A,+B] {
  import Tag._

  /** tag this as a validation */
  def validation[AA >: A, BB >: B]: Sum[AA,BB] @@ Validation = Tag.of[Validation](this)

  /** tag this as a win or lose */
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

  // preferred alias for a sum tagged as a Validation
  type V[A,B] = Sum[A,B] @@ Validation

  // preferred alias for a sum type tagged as WinLose
  type \/[A,B] = Sum[A,B] @@ WinLose

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

// these just exist in order to be tags
trait Validation
trait WinLose

trait WinLoseInstances {
  import Sum._
  implicit def winLoseMonad[Err] = new Monad[({type λ[α]=Sum[Err,α] @@ WinLose})#λ] {
    override def point[A](a: => A) = Right(a).winLose
    override def flatMap[A,B](fa: Err \/ A)(f: A=>Sum[Err,B] @@ WinLose):Sum[Err,B] @@ WinLose = 
      (Tag.unwrap(fa).flatMap((x:A) => Tag.unwrap(f(x)))).winLose
  }
}

trait ValidationInstances  {
  import Sum._

  implicit def validationApplicative[Err : Semigroup] = new Applicative[({type λ[α]=Sum[Err,α] @@ Validation})#λ] {
    override def point[A](a: => A) = Right(a).validation
    override def map[A,B](fa: Err V A)(f: A => B): Err V B = (Tag.unwrap(fa).map((x: A) => f(x))).validation

    override def ap[A,B](fa: Err V A)(f: Err V (A => B)): Err V B = (Tag.unwrap(fa), Tag.unwrap(f)) match {
      case (Right(a), Right(f)) => Valid(f(a))
      case (e @ Left(_), Right(_)) => e.validation
      case (Right(a), e @ Left(_)) => e.validation
      case (Left(e1), Left(e2)) => Invalid(Semigroup[Err].append(e1, e2))
    }
  }

}
