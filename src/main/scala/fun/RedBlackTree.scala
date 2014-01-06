package fun

import scalaz.Order
import scalaz.Ordering._

trait RedBlackTree[A] {
  import RedBlackTree._

  def color: Color
  def member(a: A)(implicit order: Order[A]): Boolean

  def insert(newA: A)(implicit order: Order[A]): RedBlackTree[A] = this match {
    case RedBlackNil() ⇒ RedBlackNode(newA, Red, RedBlackNil(), RedBlackNil())
    case RedBlackNode(a, color, left, right) ⇒ order(newA, a) match {
      case LT ⇒ balancel(a, color, left.insert(newA), right)
      case GT ⇒ balancer(a, color, left, right.insert(newA))
      case EQ ⇒ this
    }

  }

  def toList(implicit order: Order[A]): Lst[A] = this match {
    case RedBlackNil() ⇒ End()
    case RedBlackNode(a, _, left, right) ⇒ left.toList ++ (a :: right.toList)
  }
}

object RedBlackTree {
  def empty[A]: RedBlackTree[A] = RedBlackNil()

  def _balancedl[A](a: A,
                   ll: RedBlackTree[A], lr: RedBlackTree[A], la: A,
                   right: RedBlackTree[A]) =
    RedBlackNode(a, Red, 
                 RedBlackNode(la, Black, ll, lr),
                 right)

  def _balancedb[A](a: A,
                   ll: RedBlackTree[A], lr: RedBlackTree[A], la: A,
                   rl: RedBlackTree[A], rr: RedBlackTree[A], ra: A) = 
    RedBlackNode(a, Red, 
                 RedBlackNode(la, Black, ll, lr),
                 RedBlackNode(ra, Black, rl, rr))

  def _balancedr[A](a: A,
                   left: RedBlackTree[A],
                   rl: RedBlackTree[A], rr: RedBlackTree[A], ra: A) = 
    RedBlackNode(a, Red, 
                 left,
                 RedBlackNode(ra, Black, rl, rr))

  def balancel[A](a: A, color: Color, left: RedBlackTree[A], right: RedBlackTree[A])(implicit order: Order[A]): RedBlackTree[A] = 
    if(color == Black) {
      left match {
        case RedBlackNode(la, Red, RedBlackNode(lla, Red, lll, llr), lr) ⇒
          _balancedl(a, lll, llr, lla, right)
        case RedBlackNode(la, Red, ll, RedBlackNode(lra, Red, lrl, lrr)) ⇒
          _balancedb(lra, ll, lrl, la, lrr, right, a)
        case _ ⇒ RedBlackNode(a, color, left, right)
      }
    } else RedBlackNode(a, color, left, right)

  def balancer[A](a: A, color: Color, left: RedBlackTree[A], right: RedBlackTree[A])(implicit order: Order[A]): RedBlackTree[A] = 
    if(color == Black) {
      right match {
        case RedBlackNode(la, Red, rl, RedBlackNode(rra, Red, rrl, rrr)) ⇒
          _balancedr(a, left, rrl, rrr, rra)
        case RedBlackNode(ra, Red, RedBlackNode(rla, Red, rll, rlr), rr) ⇒
          _balancedb(rla, left, rll, a, rlr, rr, ra)
        case _ ⇒ RedBlackNode(a, color, left, right)
      }
    } else RedBlackNode(a, color, left, right)


  sealed trait Color
  case object Red extends Color
  case object Black extends Color
  case object Empty extends Color

}

case class RedBlackNode[A](value: A, color: RedBlackTree.Color, left: RedBlackTree[A], right: RedBlackTree[A]) extends RedBlackTree[A] {
  def member(a:A)(implicit order: Order[A]): Boolean = order(a, value) match {
    case EQ ⇒ true
    case GT ⇒ right.member(a)
    case LT ⇒ left.member(a)
  }
}

case object RedBlackNil extends RedBlackTree[Nothing] {
  val color = RedBlackTree.Empty
  def member(a: Nothing)(implicit order: Order[Nothing]) = false

  def apply[A](): RedBlackTree[A] = this.asInstanceOf[RedBlackTree[A]]
  def unapply[A](a: RedBlackTree[A]): Boolean = a.color == RedBlackTree.Empty
}
