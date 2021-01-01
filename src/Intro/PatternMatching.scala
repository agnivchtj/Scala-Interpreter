package Intro

object PatternMatching {
  /**
    * We're going to start by showing you how to create a very simple non-recursive case class.
    * The code below represents booleans that can have one of two concrete values, either Truthy or Falsy.
    *
    * It's important to always use the 'sealed abstract' and 'extends' keywords when creating case classes.
    * The 'sealed' keyword makes it impossible to add more case classes to an existing type later on. This
    * prevents us from accidentally making partial functions, as explained later.
    */
  sealed abstract class Booly
  case class Truthy() extends Booly
  case class Falsy() extends Booly

  /**
    * Case classes alone are not very useful, so let's make a function that creates a Booly from an Int.
    * When creating an instance of a case class, you should always use brackets ().
    */
  def intToBooly(n: Int): Booly = {
    if (n == 1) Truthy()
    else if (n == 0) Falsy()
    else throw new IllegalArgumentException
  }

  /**
    * Here comes the first example of pattern matching.
    * We will create a function that takes a Booly and converts it back to an Int.
    *
    * The 'match' keyword announces the start of a pattern match,
    * the 'case' keyword declares what to do if the matched argument has the correct type.
    */
  def boolyToInt(b: Booly): Int = b match {
    case Falsy()  => 0
    case Truthy() => 1
  }

  /**
    * The following is an example of a so called 'partial' function. It only pattern matches on
    * one of the two possible values for Booly and will throw a nasty exception when applied to
    * the other value (Truthy).
    *
    * The compiler is nice enough to give warnings if you accidentally forget a case and create
    * a partial function.
    *
    * This is also why the 'sealed' keyword in the declaration of case classes is important.
    * Without that keyword, the compiler cannot guarantee that Falsy and Truthy are the only
    * possible cases for Booly, as we could have added another case class after compiling
    * this file.
    */
  def veryBadBoolyToInt(b: Booly): Int = b match {
    case Falsy() => 0
    case Truthy() => 1
  }


  /**
    * Represents all possible outcomes of a die roll.
    */
  sealed abstract class Die
  case class One() extends Die
  case class Two() extends Die
  case class Three() extends Die
  case class Four() extends Die
  case class Five() extends Die
  case class Six() extends Die

  /**
    * EXERCISE:
    * Correct the following function such that it returns true for any Die value >3, and false otherwise.
    * The function should work for any possible Die value and it should not produce any compiler warnings.
    * Note that we have provided you with tests in the "Test" tab. You can use these tests to check your solution.
    *
    * Hints:
    *     `case _ => ...` specifies what to do if all previous matches fail.
    *     The OR-operator, `case A() | B() => ...` can be used to assign one outcome to multiple cases.
    */
  def isItGreaterThanThree(d: Die): Boolean = d match {
    case One() | Two() | Three() => false
    case _ => true
  }
}
