package Intro

import java.util.NoSuchElementException

object ScalaLists {
  /**
    * There are several ways to create a List:
    */
  val as = List(1,2,3)
  val bs = 1 :: 2 :: 3 :: Nil
  /**      ^1   ^^^^^^^^^^^^^2
    * This second line uses the :: function to append it's left argument to it's right argument.
    * (1) is the element to be appended,
    * (2) is an instance of a List
    *
    * Nil is the constructor for an empty list.
    *
    * Therefore we could also do the following:
    */
  val cs = 1 :: List(2,3,4)

  /**
    * Watch out, the following two expressions do not create one list,
    * but rather a list containing integers and lists!
    */
  val ds = List(1, 2) :: List(3, 4) // = List(List(1,2),3,4)
  val es = 1 :: List(2, 3) :: 4 :: List(5, 6) :: List(7) // = List(1,List(2,3),4,List(5,6),7)

  /**
    * If you wish to concat two (or more) lists, use the ::: operator
    */
  val fs = List(1, 2) ::: List(3, 4) // = List(1, 2, 3, 4)

  /**
    * To pattern match on a list, we can use the :: function as illustrated above.
    * Note that any part of the pattern match that we do not use can be ignored with an underscore '_'
    * This way it is not required to give each part of a match a name.
    *
    * Note: The [E] in the function signature introduces a type variable for the generic type of List.
    * This means the head function works for any list, not just List[Int] or List[String].
    */
  def head[E](xs: List[E]): E = xs match {
    case Nil       => throw new NoSuchElementException
    case head :: _ => head
  }

  /**
    * It's also possible to pattern match on concrete values.
    * This function matches any list that starts with 1, 2 and 3 as it's first elements:
    */
  def headIs123(xs: List[Int]): Boolean = xs match {
    case 1 :: 2 :: 3 :: _ => true
    case _                => false
  }

  /**
    * EXERCISE:
    * Define a function sum(xs).
    * It should sum every second and third element in a list, ignoring all other elements.
    * Use pattern matching!
    *
    * So:
    *   sum(List(1,2,3,4,5,6)) == 2+3 + 5+6
    *   sum(List(1,2,3,4,5)) == 2+3
    *   sum(List(1,2)) == 0
    */
  def sum(xs: List[Int]): Int = xs match {
    case x1 :: x2 :: x3 :: tail => x2 + x3 + sum(tail)
    case _ => 0
  }


  /**
    * A nice feature of Scala is the possibility to treat functions in the same manner as values.
    * This means we can pass functions as arguments, and return functions from within functions.
    *
    * A demonstration of a so called 'higher order function':
    */
  def transformInt(f: (Int => Int), i: Int): Int = f(i)
  //               ^^^^^^^^^^^^^^1                 ^^^^2
  // 1: Argument f is a function that takes in Int and returns an Int
  // 2: Here we apply the function f to i, and return its result.

  /**
    * Higher order functions will be explained with more detail in class. However, to understand
    * some of the functions in the List class, you should know that when you see:
    *     f: A => B
    * in the Scala documentation, this means that the argument or variable f is a function.
    *
    * For example, the List class has a function called 'map':
    * https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#map[B](f:A=%3EB):List[B]
    *
    * The map function lets you transform every element inside a list, without using a for-loop!
    * The argument of map is a function that takes a list element, and returns something else.
    *
    */
  val xs = List(1,2,3,4,5,6,7,8,9,10)
  def plusOne(x: Int): Int = x + 1
  val timesTwo = (x: Int) => x * 2

  val ys = xs.map(plusOne) // = List(2,3,4,5,6,7,8,9,10,11)
  val zs = xs.map(timesTwo) // = List(2,4,6,8,10,12,14,16,18,20)

  /**
    * EXERCISE:
    * Define your own version of map! Loops are prohibited, so use recursion.
    */
  def myMap[A,B](xs: List[A], f: (A => B)): List[B] = xs match {
    case Nil => Nil
    case head :: tail => f(head) :: myMap(tail, f)
  }


  /**
    * EXERCISE:
    * Take a look at the takeWhile function:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#takeWhile(p:A=%3EBoolean):List[A]
    *
    * Define the function takeWhileSmallerThanFive, it should take a list and return the first n
    * elements, until the next element (n+1) is greater or equal to 5.
    *
    * Use the takeWhile function!
    */
  def takeWhileSmallerThanFive(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => Nil
      case x :: tail => if (x < 5) x :: takeWhileSmallerThanFive(tail) else Nil
    }
  }

  /**
    * EXERCISE:
    * Define the function dropWhileSmallerThanFive, it should take a list and discard the first n
    * elements, until the next element (n+1) is greater or equal to 5.
    *
    * Again, use one of Scala's built-in list functions.
    */
  def dropWhileSmallerThanFive(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => Nil
      case x :: tail => if (x < 5) dropWhileSmallerThanFive(tail) else x :: dropWhileSmallerThanFive(tail)
    }
  }

  /**
    * Take a look at the zip function:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#zip[B](that:scala.collection.GenIterable[B]):List[(A,B)]
    *
    * This function connects two lists by 'zipping' elements into tuples:
    */
  List(1,2,3,4).zip(List(2,4,6,8)) // = List( (1,2), (2,4), (3,6), (4,8) )

  /**
    * EXERCISE:
    * Define zipWith. It should zip two lists, but instead of zipping elements into a tuple,
    * it should use a function to combine two elements.
    *
    * Example: zipWith(List(1, 2, 3),
    *                  List(10, 11, 12),
    *                  (x: Int, y: Int) => x+y)
    * Should return: List(11,13,15)
    *
    * Hint: use map and zip.
    */
  def zipWith[A,B,C](xs: List[A], ys: List[B], f: (A, B) => C): List[C] = {
    (xs, ys) match {
      case (Nil, Nil) => Nil
      case (xs, Nil) => Nil
      case (Nil, ys) => Nil
      case (x :: x_tail, y :: y_tail) => f(x, y) :: zipWith(x_tail, y_tail, f)
    }
  }
}
