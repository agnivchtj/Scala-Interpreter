package Mutation

object Exercise {
  /**
    * Write a 0-argument function that sets the identifier `x` to the value
    * of the identifier `y` and returns false.
    * You can assume `x` and `y` are in scope.
    */
  val variable =
    """
      (lambda () (seq (set x y) (num= 7 0)))
    """

  /**
    * Write a 0-argument function that sets the value of the box identified by `x`
    * to the value in the box identified by `y` and returns false.
    * You can assume `x` and `y` are in scope.
    */
  val box =
    """
      (lambda () (seq (setbox x (unbox y)) (num= 7 0)))
    """
}
