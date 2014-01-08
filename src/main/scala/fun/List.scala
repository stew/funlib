package fun

import scala.{List ⇒ SList}
import annotation.tailrec
import scalaz.{MonadPlus,Monoid,Show,Cord,Equal,Cobind}

/**
  * A linked list. similar to the scala standard List (scala.collection.immutable.List)
  * 
  * One major change from the scala standard list is that this
  * structure is Invariant. I suspect that keeping the structure
  * invariant might, in a number of places, improve type inference.  I
  * am writing this partially as an experiment to see what the pros
  * and cons of an invariant list are.
  * 
  * I have purposefully left off some some functions which are found
  * in the standard List which are non-total, for example, head.
  * 
  * I also intend to try to keep the methods available to a lean set
  * of basic methods, and move other functionality to typeclasses
  * where that is convenient.
  */  
sealed trait List[A] {
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def headMaybe: Maybe[A]

  final def ::(head: A) = Cons(head, this)

  @annotation.tailrec
  final def foldl[B](f: (B,A)⇒B, b: B): B = this match {
    case End() ⇒ b
    case head Cons tail ⇒ tail.foldl(f, f(b,head))
  }

  /**
    * isomorphic to immutable.List#foldRight
    * 
    * this one is uncurried, and taking the arguments in the opposite
    * order because it will aid in type inference.  I can count on
    * zero hands how many times I have actually curried a call to
    * foldRight, and the poor type inference is easy to get tripped up
    * on.  For example:
    * collection.immutable.List(1,2,3).foldRight(None)((l,r) ⇒ r match { case Some(x) => Some(x+l) ; case None => Some(l) }
    * fails to infer the result type as Option[Int], it is inferred
    * instead as None because of the first parameter list, this
    * version will not have this problem.
    */
  def foldr[B](f: (A,B)⇒B, b: B): B = this.reverse.foldl(((x: B,y: A) ⇒ f(y,x)), b)
  // TODO should foldr be moved to just be in Foldable?

  final def reverse: List[A] = foldl[List[A]](((a,b) ⇒ b :: a), End())

  def collect[B](pf: PartialFunction[A,B]): List[B] = foldr[List[B]](((a,b) ⇒ if(pf.isDefinedAt(a)) pf(a) :: b else b), End())

  def filter[B](f: A⇒Boolean): List[A] = foldr[List[A]](((a,b) ⇒ if(f(a)) a :: b else b), End())

  def take(n: Int): List[A] = {
    def take_(n: Int, from: List[A], res: List[A]): List[A] = (n, from) match {
      case (0, _) ⇒ res.reverse
      case (_, End()) ⇒ res.reverse
      case (n, x Cons xs) ⇒ take_(n-1, xs, x :: res)
    }
    take_(n, this, End())
  }

  def drop(n: Int): List[A] = (n, this) match {
    case (0, _) ⇒ this
    case (_, End()) ⇒ this
    case (n, x Cons xs) ⇒ xs take (n-1)
  }

  /**
    * append a list to the tail of this list, this is a O(n) operation
    */
  def ++[B >: A](b: List[B]): List[B] = foldr(Cons.apply[B], b)

  /**
    * convert this list to a standard library List
    */
  def toList: SList[A] = foldr[SList[A]]((_ :: _), Nil)
}
  
/**
  * A list which is guaranteed not to be empty
  */
final case class Cons[A](head: A, tail: List[A]) extends List[A] {
  override def isEmpty = false
  override def nonEmpty = true
  override def headMaybe = There(head)
}

/**
  * the singleton which is the terminal element of every list
  */
final case object End extends List[Nothing] {
  override val isEmpty = true
  override val nonEmpty = false
  override val headMaybe = NotThere

  def unapply[A](l: List[A]) = l.isEmpty
  def apply[A]() = this.asInstanceOf[List[A]] // YOLO
}

object List {
  /**
    * construct a list from a standard library List
    */
  def fromList[A](l: SList[A]): List[A] = l.foldRight[List[A]](End())(_ :: _)

  // TODO missing instances: Foldable / Traverse, Order

  implicit def listEqual[A](implicit A0: Equal[A]): Equal[List[A]] = new ListEqual[A] {
    implicit def A = A0
  }

  implicit val listInstances: MonadPlus[List] = new MonadPlus[List] with Cobind[List]{
    override def empty[A] = End()
    override def point[A](a: ⇒ A) = Cons(a, End())
    override def plus[A](a: List[A], b: ⇒ List[A]) = a ++ b
    override def bind[A,B](fa: List[A])(f: A⇒List[B]) = fa.foldr[List[B]]({(i,r) ⇒ f(i) ++ r}, End())
    override def map[A,B](fa: List[A])(f: A⇒B) = fa.foldr[List[B]](((i,r) ⇒ f(i) :: r), End())

    override def cobind[A, B](fa: List[A])(f: List[A] => B) =
      fa match {
        case End => End()
        case _ Cons t => f(fa) :: cobind(t)(f)
      }
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def append(fa: List[A], fb: ⇒ List[A]) = fa ++ fb
    def zero: List[A] = End()
  }

  implicit def listShow[A: Show]: Show[List[A]] = new Show[List[A]] {
    override def show(as: List[A]) = {
      def commaSep(rest: List[A], acc: Cord): Cord =
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

private trait ListEqual[A] extends Equal[List[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: List[A], a2: List[A]) = (a1,a2) match {
    case (End(),End()) ⇒ true
    case (x Cons xs, y Cons ys) ⇒ A.equal(x,y) && equal(xs, ys)
    case (_, _) ⇒ false
  }
}
