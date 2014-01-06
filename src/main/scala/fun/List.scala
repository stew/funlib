package fun

import annotation.tailrec
import scalaz.{MonadPlus,Monoid,Show,Cord,Equal,Cobind}

sealed trait Lst[A] {
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def headMaybe: Maybe[A]

  final def ::(head: A) = Cons(head, this)

  @annotation.tailrec
  final def foldl[B](f: (B,A)⇒B, b: B): B = this match {
    case End() ⇒ b
    case head Cons tail ⇒ tail.foldl(f, f(b,head))
  }

  def foldr[B](f: (A,B)⇒B, b: B): B = this.reverse.foldl(((x: B,y: A) ⇒ f(y,x)), b)
  final def reverse: Lst[A] = foldl[Lst[A]](((a,b) ⇒ b :: a), End())

  def collect[B](pf: PartialFunction[A,B]): Lst[B] = foldr[Lst[B]](((a,b) ⇒ if(pf.isDefinedAt(a)) pf(a) :: b else b), End())
  def filter[B](f: A⇒Boolean): Lst[A] = foldr[Lst[A]](((a,b) ⇒ if(f(a)) a :: b else b), End())

  def ++[B >: A](b: Lst[B]): Lst[B] = foldr(Cons.apply[B], b)

  def toList: List[A] = foldr[List[A]]((_ :: _), Nil)
}
  
final case class Cons[A](head: A, tail: Lst[A]) extends Lst[A] {
  override def isEmpty = false
  override def nonEmpty = true
  override def headMaybe = There(head)
}

final case object End extends Lst[Nothing] {
  override val isEmpty = true
  override val nonEmpty = false
  override val headMaybe = NotThere

  def unapply[A](l: Lst[A]) = l.isEmpty
  def apply[A]() = this.asInstanceOf[Lst[A]] // YOLO
}


object Lst {
  def fromList[A](l: List[A]): Lst[A] = l.foldRight[Lst[A]](End())(_ :: _)

  implicit def listEqual[A](implicit A0: Equal[A]): Equal[Lst[A]] = new LstEqual[A] {
    implicit def A = A0
  }

  implicit val listInstances: MonadPlus[Lst] = new MonadPlus[Lst] with Cobind[Lst]{
    override def empty[A] = End()
    override def point[A](a: ⇒ A) = Cons(a, End())
    override def plus[A](a: Lst[A], b: ⇒ Lst[A]) = a ++ b
    override def bind[A,B](fa: Lst[A])(f: A⇒Lst[B]) = fa.foldr[Lst[B]]({(i,r) ⇒ f(i) ++ r}, End())
    override def map[A,B](fa: Lst[A])(f: A⇒B) = fa.foldr[Lst[B]](((i,r) ⇒ f(i) :: r), End())

    override def cobind[A, B](fa: Lst[A])(f: Lst[A] => B) =
      fa match {
        case End => End()
        case _ Cons t => f(fa) :: cobind(t)(f)
      }
  }

  implicit def listMonoid[A]: Monoid[Lst[A]] = new Monoid[Lst[A]] {
    def append(fa: Lst[A], fb: ⇒ Lst[A]) = fa ++ fb
    def zero: Lst[A] = End()
  }

  implicit def listShow[A: Show]: Show[Lst[A]] = new Show[Lst[A]] {
    override def show(as: Lst[A]) = {
      def commaSep(rest: Lst[A], acc: Cord): Cord =
        rest match {
          case End => acc
          case x Cons xs => commaSep(xs, (acc :+ ",") ++ Show[A].show(x))
        }
      "[" +: (as match {
        case End => Cord()
        case x Cons xs => commaSep(xs, Show[A].show(x))
      }) :+ "]"
    }
  }
}

private trait LstEqual[A] extends Equal[Lst[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Lst[A], a2: Lst[A]) = (a1,a2) match {
    case (End(),End()) ⇒ true
    case (x Cons xs, y Cons ys) ⇒ A.equal(x,y) && equal(xs, ys)
    case (_, _) ⇒ false
  }
}
