package Intro

object BST {
  /**
    * We will first define a slightly less trivial case class,
    * representing a binary search tree.
    *
    * Any tree consists of two types of nodes:
    * leaves which are external nodes, and internal nodes containing a root, left and right subtrees.
    *
    * Notice that the structure of these case classes is recursive.
    */
  sealed abstract class Tree
  case class Leaf() extends Tree
  case class Node(elem: Int, left: Tree, right: Tree) extends Tree

  /**
    * EXERCISE:
    * Define the case classes for Tree.
    * A tree can be a Leaf, or a Node with a value and two child trees.
    * The height function serves as an example for how the tree structure should be organized
    */

  def height(tree: Tree): Int = tree match {
    case Leaf() => 0
    case Node(element, l, r) => 1 + Math.max(height(l), height(r))
  }


  /**
    * EXERCISE:
    * Define the following functions.
    *
    * Do not worry about rebalancing the tree.
    *
    * Hint:
    * Pattern matches can be made more specific with arbitrary conditions:
    *     `case A(n) if n>3 => print("n is greater than 3!")`
    */
  def insert(e: Int, t: Tree): Tree = {
    (e, t) match {
      case (e, Leaf()) => Node(e, Leaf(), Leaf())
      case (e, Node(element, l, r)) =>
        if (e == element) Node(element, l, r)
        else if (e < element) Node(element, insert(e, l), r)
        else Node(element, l, insert(e, r))
    }
  }

  def contains(e: Int, t: Tree): Boolean = {
    (e, t) match {
      case (e, Leaf()) => false
      case (e, Node(element, l, r)) =>
        if (e == element) true
        else if (e < element) contains(e, l)
        else contains(e, r)
    }
  }

  def size(t: Tree): Int = {
    t match {
      case Leaf() => 0
      case Node(element, l, r) => 1 + size(l) + size(r)
    }
  }
}
