package Func_Env

object Exercise {
  /**
    * Define an expression that fails under interpretation with static scope,
    * but succeeds under interpretation with dynamic scope.
    */
  def scope =
    """
        (let ((a 0) (b 1) (sum 0))
          (let
            ((f
              (lambda (n)
                (if (or (num= n 0) (num= n 1))
                  sum
                  (+ a (+ b (f (- n 1))))
                )
              )
            ))
            (f 7)
          )
        )
      """

  /**
    * Implement an eager equivalent of the Y combinator (somtimes known as the Z combinator).
    * See, e.g.,
    * https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus
    */
  def Y =
    """
        (lambda (f) ((lambda (x) (f (lambda (a) ((x x) a))))
        (lambda (x) (f (lambda (a) ((x x) a))))))
      """

  /**
    * Use your Y combinator to define and return the factorial function as a higher-order function.
    */
  //(if (num= n 0) 1 (* n (fact (- n 1))))
  def fact =
    """
        ((lambda (f) ((lambda (x) (f (lambda (a) ((x x) a))))
        (lambda (x) (f (lambda (a) ((x x) a)))))) (lambda (fact) (lambda (n)
        (if (num= n 0) 1 (* n (fact (- n 1)))))))
      """

  /**
    * Use your Y combinator to define and return the fibonacci function as a higher-order function.
    */
  //(if (num< n 2) (if (num= n 0) 0 1) (+ 0 (+ (fib (- x 1)) (fib (- x 2)))))
  def fib =
    """
        ((lambda (f) ((lambda (x) (f (lambda (a) ((x x) a))))
        (lambda (x) (f (lambda (a) ((x x) a)))))) (lambda (fib) (lambda (n)
        (if (num< n 2) (if (num= n 0) 0 1) (+ 0 (+ (fib (- n 1)) (fib (- n 2))))))))
      """
}
