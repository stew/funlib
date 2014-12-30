package fun

import scala.reflect.ClassTag
import Maybe._

/**
  * A persistent vector, similar to scala's Vector in its performance
  * characteristics
  */
class Vector[A](farray: Array[A], fnodes: VNode[A], val splitAt: Int, bnodes: VNode[A], barray: Array[A]) {
  import Vector._

  def size = splitAt + VHalf.size(barray, bnodes)
  def isEmpty: Boolean = VHalf.isEmpty(farray, fnodes) && VHalf.isEmpty(barray, bnodes)

  def nth(i: Int): Maybe[A] =
    if(i < splitAt)
      VHalf.nth(farray, fnodes, splitAt - i - 1)
    else
      VHalf.nth(barray, bnodes,i - splitAt)


  def foldl[B](f: (B,A)⇒B, b: B): B = VHalf.foldr[A,B](barray,bnodes,{(b,a) => f(a,b)},VHalf.foldl(farray,fnodes,f,b))
  def foldr[B](f: (A,=>B)⇒B, b: =>B): B = VHalf.foldl[A,B](barray,bnodes,{(a,b) => f(b,a)},VHalf.foldr(farray,fnodes,f,b))

  def +:(a: A)(implicit ct: ClassTag[A]): Vector[A] = {
    val nn = VHalf.:+(farray,fnodes,a)
    new Vector(nn._1, nn._2, splitAt+1, bnodes, barray)
  }

  def :+(a: A)(implicit ct: ClassTag[A]): Vector[A] = {
    val nn = VHalf.:+(barray,bnodes, a)
    new Vector(farray,fnodes, splitAt, nn._2, nn._1)
  }
}

object Vector {
  final val logWidth = 5
  final val width = 1 << logWidth
  final val widths = (for(i <- 0 to 25) yield((1 << logWidth) << i)).to[Array]
  final val masks = widths map (_ -1)

  def empty[A : ClassTag]: Vector[A] = new Vector(Array.empty[A], VNode.empty[A], 0, VNode.empty[A], Array.empty[A])

  implicit class RichArray[A](array: Array[A]) {
    /**
      * create a new array with the nth element replaced
      */
    def replaceN(n: Int, a: A) = {
      val newArr = array.clone
      newArr(n) = a
      newArr
    }

    def nth(n: Int): Maybe[A] =
      if(n < array.length) There(array(n)) else NotThere()

    def isFull = array.length == width

    def foldr[B](f: (A,=>B)=>B, b: =>B): B = {
      var i=0
      var res = b
      while(i<array.length) {
        res=f(array(i),res)
        i = i+1
      }
      res
    }

  }
}


/**
  * base trait for VNode and VLeaf
  */
sealed trait V[A] {
  def nth(i: Int) : Maybe[A]
  def height: Int
  def foldl[B](f: (B,A)=>B, b: B): B 
  def foldr[B](f: (A,=>B)=>B, b: =>B): B 
}

trait Grow[A]
case class Overflow[A](overflow: V[A]) extends Grow[A]
case class Replace[A](replacement: VNode[A]) extends Grow[A]

case class VNode[A](height: Int, children: Array[V[A]]) extends V[A] {
  import Vector._
  def size = children.size * (1 << (logWidth * height))
  def isEmpty = children.isEmpty

  def nth(i: Int): Maybe[A] =
    children(i >> (height * logWidth)).nth(i & masks(height))


  def foldl[B](f: (B,A)=>B, b: B): B = children.foldLeft(b)((b,c) => c.foldl(f,b))
  
  def foldr[B](f: (A,=>B)=>B, b: =>B): B = children.foldLeft(b)((b,c) => c.foldr(f,b))

  // this can obviously be better its too complicated, makes use of
  // asInstanceOf, makes allocations of Overflow and Append
  def appendLeaf(leaf: VLeaf[A]): Grow[A] = {
      if(height == 1) {
        // our children are leaves, see if we have room
        if(children.isFull)
          // no room, return it as an overflow
          Overflow(VNode(height, Array(leaf)))
        else
          // we have room tell our parents to replace us
          Replace(VNode(height, children :+ leaf))
      } else {
        // tell our child to append the leaf
        children.last.asInstanceOf[VNode[A]].appendLeaf(leaf) match { //ugh .asInstanceOf
          case Replace(n) =>
            // we need to replace our child
            Replace(VNode(height, children.replaceN(children.length-1, n)))
          case Overflow(a) =>
            // our child overflowed, see if we have room for the overflow
            if(children.isFull) {
              // nope, we overflow too
              val newArr = new Array[V[A]](1)
              newArr(0) = a
              Overflow(VNode(height, newArr))
            } else Replace(VNode(height, children :+ a))
        }
      }
  }
}

object VNode {
  def empty[A : ClassTag] = VNode(1, Array.empty[V[A]])
}

object VHalf {
  import Vector._
  def isEmpty[A](array: Array[A], node: VNode[A]) = array.isEmpty && node.isEmpty
  def size[A](array: Array[A], node: VNode[A]) = array.length + node.size

  def nth[A](array: Array[A], node: VNode[A], i: Int): Maybe[A] = if(i < node.size) node.nth(i) else array.nth(i - node.size)

  def :+[A : ClassTag](array: Array[A], node: VNode[A], a: A): (Array[A], VNode[A]) = {
    val newArray = (array :+ a).toArray
    if(newArray.isFull) 
      (Array.empty[A], appendLeaf(array, node, VLeaf(newArray)))
    else
      (newArray, node)
  }

  def foldl[A,B](array: Array[A], node: VNode[A], f: (B,A)=>B, b: B): B = node.foldl(f, array.foldLeft(b)(f))
  def foldr[A,B](array: Array[A], node: VNode[A], f: (A,=>B)=>B, b: =>B): B = array.foldr(f,node.foldr(f, b))

  def appendLeaf[A](array: Array[A], node: VNode[A], leaf: VLeaf[A]): VNode[A] = node.appendLeaf(leaf) match {
    case Replace(n) => n
    case Overflow(n) => VNode(node.height+1, Array(node, n))
  }
}

case class VLeaf[A](children: Array[A]) extends V[A] {
  import Vector._
  override def height = 0
  def nth(i: Int): Maybe[A] = There(children(i))

  def foldl[B](f: (B,A)=>B, b: B): B = children.foldLeft(b)(f)
  def foldr[B](f: (A,=>B)=>B, b: =>B): B = children.foldr(f,b)
}
