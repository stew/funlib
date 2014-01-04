package fun

import scalaz.Order
import scalaz.Ordering._
import scala.annotation.tailrec
import scalaz.syntax.monad._

case class BinomialHeap[A](trees: Array[Maybe[BinomialTree[A]]]) {
  import BinomialHeap._
  import Maybe._

  def isEmpty = trees.isEmpty
  def nonEmpty = trees.nonEmpty

  def insert(a: A)(implicit order: Order[A]): BinomialHeap[A] = insertTree(BinomialTree.singleton(a))

  def insertTree(tree: BinomialTree[A])(implicit order: Order[A]): BinomialHeap[A] = {
    val newTrees = Array.fill[Maybe[BinomialTree[A]]](Math.min(trees.length,tree.rank + 1))(NotThere)
    trees.copyToArray(newTrees)

    _insertTree(tree, newTrees)
    BinomialHeap(newTrees)
  }

  def headMaybe(implicit order: Order[A]): Maybe[A] = {
    _findMinTree(NotThere, trees, 0) map { t ⇒ t._2.value }
  }

  def uncons(implicit order: Order[A]): Maybe[(A,BinomialHeap[A])] = {
    val minTree = _findMinTree(NotThere, trees, 0)
    minTree >>= { mt ⇒ 
      val newTrees = trees.clone
      val removedOpt = trees(mt._1)
      newTrees(mt._1) = NotThere
      for {
        removed ← removedOpt
        topt ← removed.children.trees
        t ← topt
      } _insertTree(t, newTrees)
      removedOpt >>= { removed ⇒
        There((removed.value, BinomialHeap(newTrees)))
      }
    }
  }

  def toStream(implicit order: Order[A]): Stream[A] = Maybe.unfold(this)(_.uncons)
}

object BinomialHeap {
  def empty[A]: BinomialHeap[A] = BinomialHeap(new Array[Maybe[BinomialTree[A]]](0))

  @tailrec
  def _insertTree[A](tree: BinomialTree[A], newTrees: Array[Maybe[BinomialTree[A]]])(implicit order: Order[A]) {
    val oldTree = newTrees(tree.rank)
    oldTree match {
      case NotThere ⇒ newTrees(tree.rank) = There(tree)
      case There(other) ⇒
        val merged = _mergeTrees(tree, other)
        newTrees(tree.rank) = NotThere
        _insertTree(merged, newTrees)
    }
  }

  @tailrec
  def _findMinTree[A: Order](r: Maybe[(Int, BinomialTree[A])], as: Array[Maybe[BinomialTree[A]]], i: Int): Maybe[(Int, BinomialTree[A])] =
    if(i >= as.length) r
    else (r, as(i)) match {
      case (NotThere, NotThere) ⇒ _findMinTree(NotThere, as, i+1)
      case (NotThere, There(y)) ⇒ _findMinTree(There((i,y)), as, i+1)
      case (x, NotThere) ⇒ _findMinTree(x, as, i+1)
      case (There((i,x)), There(y)) ⇒ 
        if(Order[A].apply(x.value, y.value) == GT) _findMinTree(There((i,y)), as, i+1)
        else _findMinTree(r, as, i+1)
    }


  def _mergeTrees[A: Order](a: BinomialTree[A], b: BinomialTree[A]): BinomialTree[A] = {
    val newChildren : Array[Maybe[BinomialTree[A]]] = new Array[Maybe[BinomialTree[A]]](a.rank+1)
    if(Order[A].apply(a.value, b.value) == GT) {
      b.children.trees.copyToArray(newChildren, 1)
      newChildren(0) = There(a)
      BinomialTree(a.rank+1, b.value, BinomialHeap(newChildren))
    } else {
      a.children.trees.copyToArray(newChildren, 1)
      newChildren(0) = There(b)
      BinomialTree(a.rank+1, a.value, BinomialHeap(newChildren))
    }
  }
}


case class BinomialTree[A](rank: Int, value: A, children: BinomialHeap[A])

object BinomialTree {
  def singleton[A](a: A) = BinomialTree(0, a, BinomialHeap(new Array[Maybe[BinomialTree[A]]](0)))
}

