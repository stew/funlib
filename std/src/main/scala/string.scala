package fun
package std

object string extends StringInstances

trait StringInstances {
  import Comparison._

  implicit val stringOrder: Order[String] = Order.of[String] { (s,t) =>
    val c = s compareTo t
    if(c < 0) LT
    else if(c > 0) GT
    else EQ
  }
}
