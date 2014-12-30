package fun

object Id {
  type Id[+X] = X

  def apply[A](a: => A) = a

  implicit val idInstances = new Monad[Id] {
    override def point[A](a: => A) = a
    override def map[A,B](a: Id[A])(f: A=>B): B = f(a)
    override def flatMap[A,B](a: Id[A])(f: A=>B): B = f(a)
  }
}
