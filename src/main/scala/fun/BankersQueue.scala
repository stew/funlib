package fun

import scala.{Stream ⇒ SStream}

/**
  * A queue (LIFO) which has constant time append, constant time
  * peeking at head, and amortized constant time dequeue It is
  * implemented as two linked lists, one for the front of the queue,
  * and one for the back of the queue.  The constant time access to
  * the head is maintained by ensuring that if a dequeue operation
  * would empty the front list, it is then replentished by reversing
  * the back list, and using that as the new front list.  This
  * operation of reversing is O(n), but amortized over a number of
  * dequeue operations, it becomes O(1).
  */
sealed trait BankersQueue[A] {
  def isEmpty: Boolean
  def nonEmpty: Boolean

  def headMaybe: Maybe[A]

  def uncons: Maybe[(A, BankersQueue[A])] = this match {
    case EmptyBankersQueue() ⇒ NotThere()
    case FullBankersQueue(a Cons End() ,End()) ⇒ There((a, EmptyBankersQueue()))
    case FullBankersQueue(a Cons End(), ts) ⇒ There((a, FullBankersQueue(ts.reverse, End())))
    case FullBankersQueue(a Cons as, ts) ⇒ There((a, FullBankersQueue(as, ts)))
  }

  def snoc(a: A): BankersQueue[A] = this match {
    case EmptyBankersQueue() ⇒ FullBankersQueue(a :: End(), End())
    case FullBankersQueue(as, ts) ⇒ FullBankersQueue(as, a :: ts)
  }

  def toStream: Stream[A] = Stream.unfold(this)(_.uncons)
}

case class FullBankersQueue[A](heads: List[A], tails: List[A]) extends BankersQueue[A] {
  def isEmpty: Boolean = false
  def nonEmpty: Boolean = true
  def headMaybe: Maybe[A] = heads.headMaybe
}

final case object EmptyBankersQueue extends BankersQueue[Nothing] {
  val isEmpty = true
  val nonEmpty = false

  val headMaybe = NotThere

  def apply[A]() = this.asInstanceOf[BankersQueue[A]]
  def unapply[A](q: BankersQueue[A]) = q.isEmpty
}

