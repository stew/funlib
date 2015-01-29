package fun

/**
  * an alternative to Option
  * 
  * The major changes from Option are that this is invariant as an
  * experiment, and some of the non-total functions are elided, such
  * as Option#get
  */
sealed trait Maybe[A] {
  import Sum._

  def isDefined: Boolean
  def isEmpty: Boolean
  def nonEmpty: Boolean

  final def cata[X](there: A⇒X, notThere: ⇒ X): X = this match {
    case There(a) ⇒ there(a)
    case NotThere() ⇒ notThere
  }

  final def map[B](f: A => B): Maybe[B] = this match {
      case There(a) ⇒ There(f(a))
      case _ ⇒ NotThere()
  }

  final def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
      case There(a) ⇒ f(a)
      case _ ⇒ NotThere()
  }

  final def foreach(f: A ⇒ Unit): Unit = this match {
    case There(a) ⇒ f(a)
    case _ ⇒
  }

  final def filter(f: A ⇒ Boolean): Maybe[A] = this match {
    case t @ There(a) ⇒ if(f(a)) t else NotThere()
    case _ ⇒ this
  }

  final def orElse(a: ⇒ Maybe[A]): Maybe[A] = this match {
    case t @ There(_) ⇒ t
    case NotThere() ⇒ a
  }

  final def |||(a: ⇒ Maybe[A]): Maybe[A] = this orElse a

  def getOrElse[AA >: A](a: ⇒ AA): AA = this match {
    case There(t) ⇒ t
    case NotThere() ⇒ a
  }

  def |[AA >: A](a: ⇒ AA): AA = this getOrElse a

  final def toOption: Option[A] = this match {
    case There(x) ⇒ Some(x)
    case NotThere() ⇒ None
  }

  final def toRight[AA >: A, E](fa: Maybe[AA])(e: ⇒ E): E \/ AA = fa match {
    case There(a) ⇒ Win(a)
    case NotThere ⇒ Lose(e)
  }

  final def toLeft[AA >: A, B](fa: Maybe[AA])(b: ⇒ B): AA \/ B = fa match {
    case There(a) ⇒ Lose(a)
    case NotThere ⇒ Win(b)
  }

  final def toValid[A,E](fa: Maybe[A])(e: ⇒ E): E V A = fa match {
    case There(a) ⇒ Valid(a)
    case NotThere ⇒ Invalid(e)
  }

  final def toInvalid[A,B](fa: Maybe[A])(b: ⇒ B): A V B = fa match {
    case There(a) ⇒ Invalid(a)
    case NotThere ⇒ Valid(b)
  }
}

object Maybe {
  def not[A]: Maybe[A] = NotThere()

  def fromMaybeNull[A <: AnyRef](a: A): Maybe[A] = if(a == null) NotThere() else There(a)

  def fromOption[A](l: Option[A]): Maybe[A] = l match {
    case Some(x) ⇒ There(x)
    case None ⇒ NotThere()
  }

  // this totally doesn't belong here but on some stream class
  def unfold[A, B](seed: A)(f: A => Maybe[(B, A)]): Stream[B] =
    f(seed) match {
      case NotThere()         => Stream.empty
      case There((b, a)) => Stream.cons(b, unfold(a)(f))
    }

  implicit val maybeInstances: Monad[Maybe] = new Monad[Maybe] {
    override def point[A](a: ⇒ A): Maybe[A] = There(a)
    override def map[A,B](fa: Maybe[A])(f: A⇒B): Maybe[B] = fa map f

    override def flatMap[A,B](fa: Maybe[A])(f: A⇒Maybe[B]): Maybe[B] = fa match {
      case There(a) ⇒ f(a)
      case NotThere ⇒ NotThere()
    }
  }

    implicit def maybeMonoid[A:Monoid]: Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
    override def zero = NotThere()
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


final case class There[A](get: A) extends Maybe[A] {
  override def isDefined: Boolean = true
  override def isEmpty: Boolean = false
  override def nonEmpty: Boolean = true

}

final case object NotThere extends Maybe[Nothing] {
  override val isDefined: Boolean = false
  override val isEmpty: Boolean = true
  override val nonEmpty: Boolean = false

  def unapply[A](m: Maybe[A]) = m.isEmpty
  def apply[A](): Maybe[A] = this.asInstanceOf[Maybe[A]] // YOLO
}

private trait MaybeEqual[A] extends Equal[Maybe[A]] {
  implicit def A: Equal[A]

//  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Maybe[A], a2: Maybe[A]) = (a1,a2) match {
    case (NotThere(),NotThere()) ⇒ true
    case (_, NotThere()) ⇒ false
    case (NotThere(), _) ⇒ false
    case (There(x), There(y)) ⇒ A.equal(x,y)
  }
}
