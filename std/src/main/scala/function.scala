package fun
package std

trait FunctionInstances {
  implicit val function0Functor: Functor[Function0] = new Functor[Function0] {
    override def map[A,B](fa: Function0[A])(f: A=>B): Function0[B] = () => f(fa())
  }

  val function0Interp: Function0 ~> Id.Id = new (Function0 ~> Id.Id) {
    def apply[A](fa: Function0[A]): Id.Id[A] = fa()
  }
}
