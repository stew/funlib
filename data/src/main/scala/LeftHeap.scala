package fun

import Comparison._
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
sealed trait LeftHeap[A] {
  import LeftHeap._
  def isEmpty: Boolean
  def nonEmpty = !isEmpty
  def rank: Int

  def insert(a: A)(implicit order: Order[A]): LeftHeap[A] = merge(this, single(a))

  def headMaybe: Maybe[A] = this match {
    case LeftHeapNil() ⇒ NotThere()
    case LeftHeapNode(a, _, _, _) ⇒ There(a)
  }

  def uncons(implicit order: Order[A]): Maybe[(A, LeftHeap[A])] = this match {
    case LeftHeapNil() ⇒ NotThere()
    case LeftHeapNode(a, _, l, r) ⇒ There((a, merge(l,r)))
  }

  def pop(implicit order: Order[A]): LeftHeap[A] = this match {
    case LeftHeapNil() ⇒ empty[A]
    case LeftHeapNode(_, _, l, r) ⇒ merge(l,r)
  }

  def toStream(implicit order: Order[A]): Stream[A] = Stream.unfold(this)(_.uncons)
}

object LeftHeap {

  def empty[A]: LeftHeap[A] = LeftHeapNil.asInstanceOf[LeftHeap[A]] // YOLO
  def single[A](a: A): LeftHeap[A] = LeftHeapNode(a, 1, LeftHeapNil(), LeftHeapNil())

  def makeNode[A](h: A, c1: LeftHeap[A], c2: LeftHeap[A]): LeftHeap[A] =
    if(c1.rank > c2.rank)
      LeftHeapNode(h, c1.rank+1, c1, c2)
    else
      LeftHeapNode(h, c2.rank+1, c2, c1)

  def merge[A](heap1: LeftHeap[A], heap2: LeftHeap[A])(implicit order: Order[A]): LeftHeap[A] = 
    (heap1, heap2) match {
      case (LeftHeapNil(), _) ⇒ heap2
      case (_, LeftHeapNil()) ⇒ heap1
      case (LeftHeapNode(head1, rank1, left1, right1), LeftHeapNode(head2, rank2, left2, right2)) ⇒ order(head1, head2) match {
        case LT | EQ ⇒ makeNode(head1, left1, merge(heap2, right1))
        case _ ⇒ makeNode(head2, left2, merge(heap1,right2))
        }
      }

/*
  implicit def heapShow[A: Show]: Show[LeftHeap[A]] = new Show[LeftHeap[A]] {
    override def show(fa: LeftHeap[A]) = {
      fa match {
        case LeftHeapNil() ⇒ "_"
        case LeftHeapNode(v, _, l, r) ⇒ ("[" +: Show[A].show(v)) ++ (" -- " +: show(l) :+ " - ") ++ (show(r) :+ "]")
      }
    }
  }
 */
  implicit def heapMonoid[A: Order] = new Monoid[LeftHeap[A]] {
    override def zero = empty
    override def append(a: LeftHeap[A], b: ⇒ LeftHeap[A]) = merge(a, b)
  }
  
  implicit def heapEqual[A](implicit A0: Equal[A], O0: Order[A]): Equal[LeftHeap[A]] = new LeftHeapEqual[A] {
    implicit def A = A0
    implicit def O = O0
  }
}

case object LeftHeapNil extends LeftHeap[Nothing] {
  override val isEmpty = true
  override val nonEmpty = false
  override val rank = 0

  def unapply[A](a: LeftHeap[A]): Boolean = a.rank == 0
  def apply[A](): LeftHeap[A] = this.asInstanceOf[LeftHeap[A]] // YOLO
}

case class LeftHeapNode[A](head: A, rank: Int, left: LeftHeap[A], right: LeftHeap[A]) extends LeftHeap[A] {

  override val isEmpty = false

}

private trait LeftHeapEqual[A] extends Equal[LeftHeap[A]] {
  implicit def A: Equal[A]
  implicit def O: Order[A]

  override def equal(a1: LeftHeap[A], a2: LeftHeap[A]) = Equal[Stream[A]].equal(a1.toStream, a2.toStream)
}
