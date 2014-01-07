package fun

sealed trait BankersDequeue[A] {
  def isEmpty: Boolean
  def nonEmpty = !isEmpty

  def frontMaybe: Maybe[A]
  def backMaybe: Maybe[A]

  def uncons: Maybe[(A, BankersDequeue[A])] = this match {
    case EmptyBankersDequeue() ⇒ NotThere()
    case SingletonBankersDequeue(a) ⇒ There((a, EmptyBankersDequeue()))
    case FullBankersDequeue(f Cons End(), 1, End(), 0) ⇒ There((f, EmptyBankersDequeue()))
    case FullBankersDequeue(f Cons End(), 1, single Cons End(), 1) ⇒ There((f, SingletonBankersDequeue(single)))
    case FullBankersDequeue(f Cons End(), 1, back, bs) ⇒ {
      val toTake = bs / 2
      There((f, FullBankersDequeue(back take toTake, toTake, back drop toTake, bs - toTake)))
    }
    case FullBankersDequeue(f Cons fs, s, back, bs) ⇒ There((f, FullBankersDequeue(fs, s-1, back, bs)))
  }

  def unsnoc: Maybe[(A, BankersDequeue[A])] = this match {
    case EmptyBankersDequeue() ⇒ NotThere()
    case SingletonBankersDequeue(a) ⇒ There((a, EmptyBankersDequeue())) 
    case FullBankersDequeue(End(), 0, b Cons End(), 1) ⇒ There((b, EmptyBankersDequeue()))
    case FullBankersDequeue(single Cons End(), 1, b Cons End(), 1) ⇒ There((b, SingletonBankersDequeue(single)))
    case FullBankersDequeue(front, fs, b Cons End(), 1) ⇒ {
      val toTake = fs / 2
      There((b, FullBankersDequeue(front drop toTake, fs - toTake, front take toTake, toTake)))
    }
    case FullBankersDequeue(front, fs, b Cons bs, s) ⇒ There((b, FullBankersDequeue(front, fs, bs, s-1)))
  }

  def cons(a: A): BankersDequeue[A] = this match {
    case EmptyBankersDequeue() ⇒ SingletonBankersDequeue(a)
    case SingletonBankersDequeue(single) ⇒ FullBankersDequeue(a :: End(), 1, single :: End(), 1 )
    case FullBankersDequeue(front, fs, back, bs) ⇒ FullBankersDequeue(a :: front, fs+1, back, bs)
  }

  def snoc(a: A): BankersDequeue[A] = this match {
    case EmptyBankersDequeue() ⇒ SingletonBankersDequeue(a)
    case SingletonBankersDequeue(single) ⇒ FullBankersDequeue(single :: End(), 1, a :: End(), 1 )
    case FullBankersDequeue(front, fs, back, bs) ⇒ FullBankersDequeue(front, fs, a :: back, bs+1)
  }
}

final case class SingletonBankersDequeue[A](single: A) extends BankersDequeue[A] {
  def isEmpty = false
  def frontMaybe = There(single)
  def backMaybe = There(single)
}

final case class FullBankersDequeue[A](front: Lst[A], fsize: Int, back: Lst[A], backSize: Int) extends BankersDequeue[A]  {
  def isEmpty = false
  def frontMaybe = front.headMaybe
  def backMaybe = back.headMaybe
}
   
final case object EmptyBankersDequeue extends BankersDequeue[Nothing] {
  val isEmpty = true

  val frontMaybe = NotThere
  val backMaybe = NotThere

  def apply[A]() = this.asInstanceOf[BankersDequeue[A]]
  def unapply[A](q: BankersDequeue[A]) = q.isEmpty
}
