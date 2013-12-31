package fun

import scalaz.{Equal, Order, Show}
import scalaz.Ordering._

/**
  * A "leftist heap".  A heap ordered binary tree with the following
  * properties: Each node is stored with its "rank".  Rank is defined
  * as being the distance from a node to an empty node doing a right
  * traversal.  It is guaranteed that he left subtree is always of
  * equal or higher rank than the right subtree.
  * 
  * Finding the min element is O(1)
  * Removing the min element is O(log n)
  * Inserting an element is O(log n)
  */
sealed trait Heap[A] {
  import Heap._
  def isEmpty: Boolean
  def nonEmpty = !isEmpty
  def rank: Int

  def insert(a: A)(implicit order: Order[A]): Heap[A] = merge(this, single(a))

  def headMaybe: Maybe[A] = this match {
    case HeapNil() ⇒ NotThere
    case HeapNode(a, _, _, _) ⇒ There(a)
  }

  def pop(implicit order: Order[A]): Heap[A] = this match {
    case HeapNil() ⇒ empty[A]
    case HeapNode(_, _, l, r) ⇒ merge(l,r)
  }

  def fold[B](f: (B,A) ⇒ B, b: B)(implicit order: Order[A], showa: Show[A]): B = this match {
    case HeapNil() ⇒ b
    case HeapNode(a, _, l, r) ⇒ merge(l,r).fold(f, f(b,a))
  }
}

object Heap {
  def empty[A]: Heap[A] = HeapNil.asInstanceOf[Heap[A]] // YOLO
  def single[A](a: A): Heap[A] = HeapNode(a, 1, HeapNil(), HeapNil())

  def makeNode[A](h: A, c1: Heap[A], c2: Heap[A]): Heap[A] =
    if(c1.rank > c2.rank)
      HeapNode(h, c1.rank+1, c1, c2)
    else
      HeapNode(h, c2.rank+1, c2, c1)

  def merge[A](heap1: Heap[A], heap2: Heap[A])(implicit order: Order[A]): Heap[A] = 
    (heap1, heap2) match {
      case (HeapNil(), _) ⇒ heap2
      case (_, HeapNil()) ⇒ heap1
      case (HeapNode(head1, rank1, left1, right1), HeapNode(head2, rank2, left2, right2)) ⇒ order(head1, head2) match {
        case LT | EQ ⇒ makeNode(head1, left1, merge(heap2, right1))
        case _ ⇒ makeNode(head2, left2, merge(heap1,right2))
        }
      }

  implicit def heapShow[A: Show]: Show[Heap[A]] = new Show[Heap[A]] {
    override def show(fa: Heap[A]) = {
      fa match {
        case HeapNil() ⇒ "_"
        case HeapNode(v, _, l, r) ⇒ ("[" +: Show[A].show(v)) ++ (" -- " +: show(l) :+ " - ") ++ (show(r) :+ "]")
      }
    }
  }

  import scalaz.Monoid
  implicit def heapMonoid[A: Order] = new Monoid[Heap[A]] {
    override def zero = empty
    override def append(a: Heap[A], b: ⇒ Heap[A]) = merge(a, b)
  }
  
  implicit def heapEqual[A](implicit A0: Equal[A], O0: Order[A]): Equal[Heap[A]] = new HeapEqual[A] {
    implicit def A = A0
    implicit def O = O0
  }
}

case object HeapNil extends Heap[Nothing] {
  override val isEmpty = true
  override val nonEmpty = false
  override val rank = 0

  def unapply[A](a: Heap[A]): Boolean = a.rank == 0
  def apply[A](): Heap[A] = this.asInstanceOf[Heap[A]] // YOLO
}

case class HeapNode[A](head: A, rank: Int, left: Heap[A], right: Heap[A]) extends Heap[A] {

  override val isEmpty = false

}

private trait HeapEqual[A] extends Equal[Heap[A]] {
  implicit def A: Equal[A]
  implicit def O: Order[A]

  override def equalIsNatural: Boolean = A.equalIsNatural
  
  override def equal(a1: Heap[A], a2: Heap[A]) = (a1,a2) match {
    case (HeapNil(),HeapNil()) ⇒ true
    case (HeapNode(v1, _, _, _), HeapNode(v2, _, _, _)) ⇒ A.equal(v1, v2) && equal(a1.pop, a2.pop)
    case (_, _) ⇒ false
  }
}
