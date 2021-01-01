package Intro

object Lists {
  /**
    * We will now define a slightly less trivial case class,
    * representing a list structure containing integers.
    *
    * Any list can be one of two types:
    * it can be empty, or it can be an element joined to the remaining list.
    *
    * Notice that the structure of these case classes is recursive.
    */
  sealed abstract class IntList
  case class Empty()                        extends IntList // The empty list, often called Nils
  case class Element(n: Int, tail: IntList) extends IntList // Element is usually called Cons

  /**
    * As an example, let's create a function that lists descending integers from n to 1.
    */
  def listFrom(n: Int): IntList = {
    if (n == 0) Empty()
    else Element(n, listFrom(n-1))
  }

  /**
    * EXERCISE:
    * Implement the function sumIntList(xs).
    * It should take an IntList, and return the sum of all it's elements.
    * Use pattern matching!
    */
  def sumIntList(xs: IntList): Int = {
    xs match {
      case Empty() => 0
      case Element(x, tail) => x + sumIntList(tail)
    }
  }

  /**
    * EXERCISE:
    * Implement the function head(xs).
    * It should return the first element in a list.
    * If the list is empty, throw a NoSuchElementException.
    */
  def head(xs: IntList): Int = {
    xs match {
      case Empty() => throw new NoSuchElementException
      case Element(x, tail) => x
    }
  }

  /**
    * EXERCISE:
    * Define the function tail(xs).
    * It should accept an IntList and return the same IntList, but without the first element.
    * If the list is empty, throw a NoSuchElementException.
    */
  def tail(xs: IntList): IntList = {
    xs match {
      case Empty() => throw new NoSuchElementException
      case Element(x, body) => body
    }
  }

  /**
    * EXERCISE:
    * Define the function concat(xs, ys).
    * It should concatenate two IntLists.
    */
  def concat(xs: IntList, ys: IntList): IntList = {
    xs match {
      case Empty() => ys
      case Element(x, tail) => Element(x, concat(tail, ys))
    }
  }

  /**
    * EXERCISE:
    * Define the function take(n, xs).
    * It should return the first n elements of xs.
    * This function should never throw an exception.
    */
  def take(n: Int, xs: IntList): IntList = {
    (n, xs) match {
      case (n, Empty()) => Empty()
      case (0, xs) => Empty()
      case (n, Element(x, tail)) => Element(x, take(n - 1, tail))
    }
  }

  /**
    * EXERCISE:
    * Define the function drop(n, xs).
    * It should return the list xs, without the first n elements.
    * This function should never throw an exception.
    */
  def drop(n: Int, xs: IntList): IntList = {
    (n, xs) match {
      case (n, Empty()) => Empty()
      case (0, xs) => xs
      case (n, Element(x, tail)) => drop(n - 1, tail)
    }
  }
}
