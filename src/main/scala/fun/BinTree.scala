package fun

import scalaz.Order

/*
sealed trait BinTree[A] {
  def isEmpty: Boolean
  def nonEmpty: Boolean

  def insert(a: A: Order): BinTree[A] = this match {
    case TreeNil() ⇒ TreeNode(a, BinTree.Empty, BinTree.Empty)
    case TreeNode(a1: A, left, right) ⇒ Order[A](a, a1) match {

    }
  }
}

object BinTree {

  private case class TreeNode[A](value: A, left: BinTree[A], right: BinTree[A]): BinTree[A] {
    override val isEmpty = false
    override val nonEmpty = true
  }

  private case object TreeNil extends BinTree[Nothing] {
    override val isEmpty = true
    override val nonEmpty = false

    def unapply[A](a: BinTree[A]): Boolean = a.isEmpty == true
    def apply[A](): BinTree[A] = this.asInstanceOf[BinTree[A]] // YOLO
  }
}

*/
