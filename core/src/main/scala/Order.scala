package fun

trait Order[A] {
  def apply(left: A, right: A): Comparison
}

object Order {
  def apply[A](implicit ev: Order[A]): Order[A] = implicitly

  def of[A](f: (A,A) => Comparison): Order[A] = new Order[A] {
    def apply(l: A, r: A) = f(l,r)
  }

  implicit val contraOrder: ContraFunctor[Order] = new ContraFunctor[Order] {
    override def contramap[A,B](o: Order[A])(f: B => A) = new Order[B] {
      def apply(left: B, right: B) = o(f(left),f(right))
    }
  }
}

sealed trait Comparison

object Comparison {
  case object GT extends Comparison
  case object EQ extends Comparison
  case object LT extends Comparison
}
