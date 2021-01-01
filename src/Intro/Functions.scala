package Intro

/**
  * In these exercises we will demonstrate some of the basic Scala syntax.
  */
object Functions {
  /**
    * To define a function, Scala uses the 'def' keyword.
    * The following function is for demonstration purposes.
    */
  def someFunc(argument: String, anotherArg: Boolean): List[Int] =  List(1, 2, 3, 4)
  /** ^1       ^2        ^3                            ^4        ^5 ^6
    *
    * Let's elaborate each term in this function declaration:
    * 1: The name of the function
    * 2: The name of the first argument
    * 3: The type of the first argument. Types in Scala are always declared with a single colon (:)
    * 4: The return type of the function is a List with Ints
    * 5: The = sign separates the function signature from its implementation
    * 6: The implementation of this function always returns a list with 1, 2, 3, 4 in it.
    *    Notice that we do not have use the return keyword!
    */

  /**
    * Calling a function with arguments can be done in two ways.
    * We can use the exact same syntax as in Java, or we can explicitly name our arguments.
    * These two ways do not differ in functionality, but naming arguments can sometimes make things more clear.
    *
    * Notice, in Scala you can omit the semicolon (;) that you are so used to from Java.
    */
  val p = someFunc("Hi", true)
  val q = someFunc(argument = "Hi", anotherArg = true)

  /**
    * The previous function only had one expression of code in its implementation.
    * When you create functions that span multiple lines, you use curly brackets to
    * indicate the start and end of your implementation.
    *
    * Again, we can omit the return keyword as long as the returned value is the
    * last evaluated expression of the function.
    */
  def multiLineFunction(): Int = {
    val a = 1+1
    val b = a+a
    val c = b+b
    c+c
  }

  /**
    * Calling a function without any arguments can also be done in two ways.
    * One way is the same as in Java (except for the trailing semicolon)
    * The other way is to omit the parentheses.
    */
  val r = multiLineFunction()
  val s = multiLineFunction


  /**
    * IMPORTANT NOTE ON VARIABLES:
    * The function above uses the 'val' keyword to create a 'value'.
    * Scala also has the 'var' keyword, which creates a 'variable'.
    *
    * The difference is that values are immutable, meaning you can never change them.
    * Variables on the other hand, can be changed after they have been declared.
    *
    * Immutable values give good software engineering benefits over their mutable counterpart:
    * data that should not need to change should be declared as immutable.
    * In general in this course we will apply a "principle of least power", and mainly work
    * in the functional programming subset of Scala, i.e., working with immutable data.
    *
    * Unless an exercise specifically calls for mutation, you should avoid it and
    * always use 'val' instead of 'var'.
    *
    * (Side note: the "principle of least power" is also the name of a popular style guide
    * for Scala programming: http://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html)
    */
  val x = 3
  // x = x + 2  // Will not compile!

  var y = 4
  y = y + 3 // Legal, but not in this course


  /**
    * EXERCISE:
    * Implement the function min(a, b), that takes two integers and returns the smallest of the two.
    * We have implemented a single test under the "Test" tab which provides a hint on how to handle
    * arguments that are equal.
    */
  def min(a: Int, b: Int): Int = {
    if (a < b) a else b
  }

  /**
    * EXERCISE:
    * Implement the function factorial(n) that returns the factorial of an integer n.
    * It should work for any integer greater or equal to 0.
    * You are encouraged (but not required) to write your own tests for this function.
    */
  def factorial(n: Int): Int = {
    if (n < 0) throw new ArithmeticException
    else if (n == 0 || n == 1) 1
    else n * factorial(n-1)
  }

  /**
    * EXERCISE:
    * Implement the fibonacci function. You should use recursion.
    * The function should return the nth number in the Fibonacci sequence.
    * You are encouraged (but not required) to write your own tests for this function.
    */
  def fibonacci(n: Int): Int = {
    //0, 1, 1, 2, 3, 5, 8, 13, 21 ...
    if (n < 0) throw new ArithmeticException
    if (n == 0) 0
    else if (n == 1) 1
    else fibonacci(n-1) + fibonacci(n-2)
  }
}
