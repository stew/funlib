package fun
package test

import org.scalameter.api._

object PrependPerformance extends PerformanceTest.Quickbenchmark {

  trait Prepend {
    type F[_]
    def empty: F[Int]
    def prepend(fa: F[Int], a: Int): F[Int]
  }

  object Prepend {
    val listPrepend: Prepend = new Prepend {
      override def toString = "fun.List"
      type F[A] = fun.List[A]
      def empty: List[Int] = End()
      def prepend(fa: fun.List[Int], a: Int): fun.List[Int] = a :: fa
    }
    val slistPrepend: Prepend = new Prepend {
      override def toString = "scala.collection.immutable.List"
      type F[A] = scala.collection.immutable.List[A]
      def empty: scala.collection.immutable.List[Int] = Nil
      def prepend(fa: scala.collection.immutable.List[Int], a: Int): scala.collection.immutable.List[Int] = a :: fa
    }
    val vectorPrepend: Prepend = new Prepend {
      override def toString = "fun.Vector prepend"
      type F[A] = fun.Vector[A]
      def empty: fun.Vector[Int] = Vector.empty[Int]
      def prepend(fa: fun.Vector[Int], a: Int): fun.Vector[Int] = a +: fa
    }
    val svectorPrepend: Prepend = new Prepend {
      override def toString = "scala.collection.immutable.Vector prepend"
      type F[A] = scala.collection.immutable.Vector[A]
      def empty: scala.collection.immutable.Vector[Int] = scala.collection.immutable.Vector.empty[Int]
      def prepend(fa: scala.collection.immutable.Vector[Int], a: Int): scala.collection.immutable.Vector[Int] = a +: fa
    }
    val vectorAppend: Prepend = new Prepend {
      override def toString = "fun.Vector append"
      type F[A] = fun.Vector[A]
      def empty: fun.Vector[Int] = Vector.empty[Int]
      def prepend(fa: fun.Vector[Int], a: Int): fun.Vector[Int] = fa :+ a
    }
    val svectorAppend: Prepend = new Prepend {
      override def toString = "scala.collection.immutable.Vector append"
      type F[A] = scala.collection.immutable.Vector[A]
      def empty: scala.collection.immutable.Vector[Int] = scala.collection.immutable.Vector.empty[Int]
      def prepend(fa: scala.collection.immutable.Vector[Int], a: Int): scala.collection.immutable.Vector[Int] = fa :+ a
    }
  }

  val containers = Gen.enumeration("container")(Prepend.listPrepend,
                                                Prepend.slistPrepend,
                                                Prepend.vectorPrepend,
                                                Prepend.svectorPrepend,
                                                Prepend.vectorAppend,
                                                Prepend.svectorAppend)

  performance of "prepend" in {
    measure method "prepend" in {
      using(containers) in { c =>
        var fa = c.empty
        for(i <- 0 to 2000000) {
          fa = c.prepend(fa,i)
        }
      }
    }
  }

}
