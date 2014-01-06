package fun

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
}

case class FullBankersQueue[A](heads: Lst[A], tails: Lst[A]) extends BankersQueue[A] {
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

