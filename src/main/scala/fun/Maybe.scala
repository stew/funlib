package fun

sealed trait Maybe[+A] {
  def isDefined: Boolean
  def isEmpty: Boolean
  def nonEmpty: Boolean

  final def cata[X](there: A⇒X, notThere: ⇒ X): X = this match {
    case There(a) ⇒ there(a)
    case NotThere ⇒ notThere
  }

  final def filter(f: A⇒Boolean): Maybe[A] = this match {
    case t @ There(a) ⇒ if(f(a)) t else NotThere
    case NotThere ⇒ NotThere
  }

  final def orElse[AA >: A](a: ⇒ Maybe[AA]): Maybe[AA] = this match {
    case t @ There(_) ⇒ t
    case NotThere ⇒ a
  }

  final def |||[AA >: A](a: ⇒ Maybe[AA]): Maybe[AA] = this orElse a

  def getOrElse[AA >: A](a: ⇒ AA): AA = this match {
    case There(t) ⇒ t
    case NotThere ⇒ a
  }

  def |[AA >: A](a: ⇒ AA): AA = this getOrElse a

  final def toOption: Option[A] = this match {
    case There(x) ⇒ Some(x)
    case NotThere ⇒ None
  }

  import scalaz.{\/,-\/,\/-,Validation,Success,Failure}
  final def toRight[AA >: A, E](fa: Maybe[AA])(e: ⇒ E): E \/ AA = fa match {
    case There(a) ⇒ \/-(a)
    case NotThere ⇒ -\/(e)
  }

  final def toLeft[AA >: A, B](fa: Maybe[AA])(b: ⇒ B): AA \/ B = fa match {
    case There(a) ⇒ -\/(a)
    case NotThere ⇒ \/-(b)
  }

  final def toSuccess[A,E](fa: Maybe[A])(e: ⇒ E): Validation[E,A] = fa match {
    case There(a) ⇒ Success(a)
    case NotThere ⇒ Failure(e)
  }

  final def toFailure[A,B](fa: Maybe[A])(b: ⇒ B): Validation[A,B] = fa match {
    case There(a) ⇒ Failure(a)
    case NotThere ⇒ Success(b)
  }
}

import scalaz.Equal
object Maybe {
  def not[A]: Maybe[A] = NotThere

  def fromMaybeNull[A <: AnyRef](a: A): Maybe[A] = if(a == null) NotThere else There(a)

  def fromOption[A](l: Option[A]): Maybe[A] = l match {
    case Some(x) ⇒ There(x)
    case None ⇒ NotThere
  }

  import scalaz.MonadPlus
  implicit val maybeInstances: MonadPlus[Maybe] = new MonadPlus[Maybe] {
    override def point[A](a: ⇒ A): Maybe[A] = There(a)
    override def map[A,B](fa: Maybe[A])(f: A⇒B): Maybe[B] = fa match {
      case There(a) ⇒ There(f(a))
      case NotThere ⇒ NotThere
    }

    override def bind[A,B](fa: Maybe[A])(f: A⇒Maybe[B]): Maybe[B] = fa match {
      case There(a) ⇒ f(a)
      case NotThere ⇒ NotThere
    }

    override def empty[A] = NotThere
    override def plus[A](a: Maybe[A], b: ⇒ Maybe[A]) = a orElse b
  }

  import scalaz.Monoid
  implicit def maybeMonoid[A:Monoid]: Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
    override def zero = NotThere
    override def append(fa: Maybe[A], fb: ⇒ Maybe[A]) = fa match {
      case There(a) ⇒ fb match {
        case There(b) ⇒ There(Monoid[A].append(a,b))
        case _ ⇒ fa
      }
      case _ ⇒ fb
    }
  }

  implicit def maybeEqual[A](implicit A0: Equal[A]): Equal[Maybe[A]] = new MaybeEqual[A] {
    implicit def A = A0
  }
}


final case class There[+A](get: A) extends Maybe[A] {
  override def isDefined: Boolean = true
  override def isEmpty: Boolean = false
  override def nonEmpty: Boolean = true

}

final case object NotThere extends Maybe[Nothing] {
  override val isDefined: Boolean = false
  override val isEmpty: Boolean = true
  override val nonEmpty: Boolean = false
}

private trait MaybeEqual[A] extends Equal[Maybe[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Maybe[A], a2: Maybe[A]) = (a1,a2) match {
    case (NotThere,NotThere) ⇒ true
    case (_, NotThere) ⇒ false
    case (NotThere, _) ⇒ false
    case (There(x), There(y)) ⇒ A.equal(x,y)
  }
}
