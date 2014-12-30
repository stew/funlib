package fun

/**
  * Very similar to BankersQueue, this is a queue which allows items
  * to be put onto either the front (cons) or the back (snoc) of the
  * queue in constant time, and constant time access to the element at
  * the very front or the very back of the queue.  Dequeueing an
  * element from either end is constant time when amorized over a
  * number of dequeues.
  * 
  * Similar to the invariant maintained in BankersQueue that says that
  * whenever the queue is non-empty, the front list must be non-empty,
  * this queue maintains an invariant that whenever there are at least
  * two elements in the queue, neither the front list nor back list
  * are empty.  In order to maintain this invariant, a dequeue from
  * either side which would leave that side empty constructs the
  * resulting queue by splitting the other list in half.
  */
sealed trait BankersDequeue[A] {
  def isEmpty: Boolean
  def nonEmpty = !isEmpty

  def frontMaybe: Maybe[A]
  def backMaybe: Maybe[A]

  /**
    * dequeue from the front of the queue
    */
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

  /**
    * dequeue from the back of the queue
    */
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

  /**
    * enqueue to the front of the queue
    */
  def cons(a: A): BankersDequeue[A] = this match {
    case EmptyBankersDequeue() ⇒ SingletonBankersDequeue(a)
    case SingletonBankersDequeue(single) ⇒ FullBankersDequeue(a :: End(), 1, single :: End(), 1 )
    case FullBankersDequeue(front, fs, back, bs) ⇒ FullBankersDequeue(a :: front, fs+1, back, bs)
  }

  /**
    * enqueue on to the back of the queue
    */
  def snoc(a: A): BankersDequeue[A] = this match {
    case EmptyBankersDequeue() ⇒ SingletonBankersDequeue(a)
    case SingletonBankersDequeue(single) ⇒ FullBankersDequeue(single :: End(), 1, a :: End(), 1 )
    case FullBankersDequeue(front, fs, back, bs) ⇒ FullBankersDequeue(front, fs, a :: back, bs+1)
  }

  /**
    * convert this queue to a stream of elements from front to back
    */
  def toStream: Stream[A] = Stream.unfold(this)(_.uncons)

  /**
    * convert this queue to a stream of elements from back to front
    */
  def toBackStream: Stream[A] = Stream.unfold(this)(_.unsnoc)

}

/**
  * special case of the queue when it contains just a single element
  * which can be accessed from either side of the queue
  */
final case class SingletonBankersDequeue[A](single: A) extends BankersDequeue[A] {
  def isEmpty = false
  def frontMaybe = There(single)
  def backMaybe = There(single)
}

/**
  * a queue which has at least two elements, it is guaranteed that the
  * front list and back lists cannot be empty
  */
final case class FullBankersDequeue[A](front: List[A], fsize: Int, back: List[A], backSize: Int) extends BankersDequeue[A]  {
  def isEmpty = false
  def frontMaybe = front.headMaybe
  def backMaybe = back.headMaybe
}
   
/**
  * a queue which has no elements
  */
final case object EmptyBankersDequeue extends BankersDequeue[Nothing] {
  val isEmpty = true

  val frontMaybe = NotThere
  val backMaybe = NotThere

  def apply[A]() = this.asInstanceOf[BankersDequeue[A]]
  def unapply[A](q: BankersDequeue[A]) = q.isEmpty
}
