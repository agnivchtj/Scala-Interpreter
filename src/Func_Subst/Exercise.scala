package Func_Subst

object Exercise {

  /** 1. Conditional Expression */
  def numerical =
    """
        (if (num= (+ x 5) 13) 0 1)
      """

  /** 2. Curried `cons` */
  //def curry[A,B,C](f: (A, B) => C): A => B => C = { x => y => f(x,y) }
  //def uncurry[A,B,C](f: A => B => C): (A, B) => C = { (x, y) => f(x)(y) }
  def curriedCons =
    """
        (lambda (x) (lambda (y) (cons x y)))
      """

  /** 3. Recursive Factorial Function */
  //(let (twice (λ (f) (λ (x) (f (f x))))) ((twice (λ (y) (+ y y))) 1))
  def fact =
    """
        (lambda (x)
          (let ((f (lambda (self x)
                (if (num= x 0)
                  1
                  (* x (self self (- x 1)))
                )
              )))
          (f f x))
        )
      """

  /** 4. Capture Avoidance */
  //(((λ (f) (λ (x) (f x))) (λ (y) (+ y x))) 21)
  def capturing =
    """
        (((lambda (f) (lambda (x) (f x))) (lambda (y) (+ y x))) 21)
      """

}
