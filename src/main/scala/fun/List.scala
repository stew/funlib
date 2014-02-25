package fun

import scala.{List ⇒ SList}
import annotation.tailrec
import scalaz.{Applicative,MonadPlus,Monoid,Show,Cord,Equal,Cobind,Foldable,Traverse,Trampoline, Free}
import Free.Trampoline
import Trampoline._

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

  final def ::(head: A): List[A] = Cons(head, this)

  @annotation.tailrec
  final def foldl[B](f: (B,A)⇒B, b: B): B = this match {
    case End() ⇒ b
    case head Cons tail ⇒ tail.foldl(f, f(b,head))
  }

  /**
    * isomorphic to immutable.List#foldRight
    * 
    * this one is uncurried, and taking the arguments in the opposite
    * order because it will aid in type inference. I find that how
    * often I want to curry a call to foldRight is small compared to
    * how oftem I'm frustrated by the poor type inference.
    * For example of infernce problems:
    * collection.immutable.List(1,2,3).foldRight(None)((l,r) ⇒ r match { case Some(x) => Some(x+l) ; case None => Some(l) }
    * fails to infer the result type as Option[Int], it is inferred
    * instead as None because of the first parameter list, this
    * version will not have this problem.
    */
  def foldr[B](f: (A,⇒ B) ⇒B, b: ⇒ B): B = {
    def foldr_(fa: List[A], b: B, f: (A, => B) => B): Trampoline[B] = 
      suspend(fa.uncons.cata((l => foldr_(l._2, b, f) map (b => f(l._1,b))), done(b)))

    foldr_(this,b,f).run
  }

  def uncons: Maybe[(A, List[A])] = this match {
    case End() => NotThere()
    case head Cons tail => There(head -> tail)
  }

  final def reverse: List[A] = foldl[List[A]](((a,b) ⇒ b :: a), End())

  def collect[B](pf: PartialFunction[A,B]): List[B] = foldl[NastyListAppend[B]](((b,a) ⇒ if(pf.isDefinedAt(a)) b += pf(a) else b), new NastyListAppend[B]).run

  def filter[B](f: A⇒Boolean): List[A] = {
    foldl[NastyListAppend[A]](((b,a) ⇒ if(f(a)) b += a else b), new NastyListAppend[A]).run
  }

  def take(n: Int): List[A] = {
    def take_(n: Int, from: List[A], res: NastyListAppend[A]): List[A] = (n, from) match {
      case (0, _) ⇒ res.run
      case (_, End()) ⇒ res.run
      case (n, x Cons xs) ⇒ 
        res += x
        take_(n-1, xs, res )
    }
    take_(n, this, new NastyListAppend)
  }

  def drop(n: Int): List[A] = (n, this) match {
    case (0, _) ⇒ this
    case (_, End()) ⇒ this
    case (n, x Cons xs) ⇒ xs take (n-1)
  }

  /**
    * append a list to the tail of this list, this is a O(n) operation
    */
  def ++[B >: A](b: List[B]): List[B] = foldr[List[B]]((_ :: _), b)

  /**
    * convert this list to a standard library List
    */
  def toList: SList[A] = foldr[SList[A]]((_ :: _), Nil)
}
  
/**
  * A list which is guaranteed not to be empty
  */
final case class Cons[A](head: A, private[fun] var _tail: List[A]) extends List[A] {
  def tail = _tail
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

  // TODO missing instances: Order

  implicit def listEqual[A](implicit A0: Equal[A]): Equal[List[A]] = new ListEqual[A] {
    implicit def A = A0
  }

  implicit val listInstances: MonadPlus[List] with Cobind[List] with Foldable[List] with Traverse[List] = new MonadPlus[List] with Cobind[List] with Foldable[List] with Traverse[List] {
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

    override def foldRight[A,B](fa: List[A], z: ⇒ B)(f: (A, ⇒ B) ⇒ B) = fa.foldr(f, z)
    override def foldLeft[A,B](fa: List[A], z: B)(f: (B, A) ⇒ B) = fa.foldl(f, z)
    override def foldMap[A,B](fa: List[A])(f: A => B)(implicit F: Monoid[B]): B = fa.foldl[B]((b,a) => F.append(b,f(a)), F.zero)

//    def traverseImpl[F[_], A, B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] = {
//      l.foldr[F[List[B]]]((a, fbs) => F.apply2(f(a), fbs)(_ :: _), F.point(empty))
//    }

    def traverseImpl[F[_], A, B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] = {
      F.map(l.foldl[F[NastyListAppend[B]]]((fbs, a) => F.apply2(fbs, f(a))(_ += _), F.point(new NastyListAppend[B])))(_.run)
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


private[fun] class NastyListAppend[A] extends collection.mutable.Builder[A,List[A]] {
  var run: List[A] = End()
  var end: Cons[A] = _

  def +=(a: A) = {
    run match {
      case End() =>
        end = Cons(a, End())
        run = end
      case _ =>
        val newEnd = Cons(a, End())
        end._tail = newEnd
        end = newEnd
    }
    this
  }

  def clear() {
    run = End()
    end = null
  }

  def result() = run
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
