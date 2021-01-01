package TypeChecker

object Exercise {
  /**
    * Write an expression that implements a well-typed factorial function.
    * The expression should have type `((Num) -> Num)`.
    */
  //(rec-lam f (x) (f x))
  //(rec-lam (f : Num -> Bool) (x) (f x))
  //(rec-lam sum (n) (if (num= n 0) 0 (+ n (sum (- n 1)))))
  def fact =
    """
        (rec-lam (f : Num -> Num) (x) (if (num= x 0) 1 (* x (f (- x 1)))))
      """

  /**
    * Write an expression of type `(Pair Num (Pair Bool ((Num) -> Num)))`.
    */
  def pairs =
    """
        (pair 1 (pair true (lambda ((x : Num)) (+ x 1))))
      """

  /**
    * Write an expression of type `(List Num)`
    */
  def list =
    """
        (list : Num (1 2))
      """

  /**
    * Write an expression of type `((Num) -> (Ref Num))`
    */
  //(letrec ((x 1)) x)
  //(letrec (((x : Num) 1)) x)
  def refFun =
    """
        (rec-lam (f : Num -> (Ref Num)) (x) (if (num= x 0) (box 1) (box 2)))
      """
}
