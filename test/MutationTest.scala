package test

//test: Test
import Parser._
import Desugar._
import Interp._
import Untyped._

import org.scalatest.FunSuite

class MutationTest extends FunSuite with CatchErrorSuite {

  /**
    * Tests for Desugaring
    */
  test("Desugar 5") {
    assertResult(
      NumC(5)
    ) {
      desugar(NumExt(5))
    }
  }

  /**
    * Tests for Interpreting
    */

  test("Interp 5") {
    assertResult(
      NumV(5)
    ) {
      interp(NumC(5))
    }
  }

  test("Interp 5+true throws InterpException") {
    intercept[InterpException] {
      interp(PlusC(NumC(5), TrueC()))
    }
  }

  /**
    * Helpers
    */
  //Let
  //LetExt(binds: List[LetBindExt], body: ExprExt)
  //LetBindExt(name: String, value: ExprExt)
  test("Parse let") {
    assertResult(
      LetExt(
        List(LetBindExt("x", NumExt(1)), LetBindExt("y", NumExt(2))),
        BinOpExt("+", IdExt("x"), IdExt("y"))
      )
    ) {
      parse("(let ((x 1) (y 2)) (+ x y))")
    }
  }

  //f(1, 2) = 3
  test("Desugar let") {
    assertResult(
      AppC(
        FdC(List("x", "y"), PlusC(IdC("x"), IdC("y"))),
        List(NumC(1), NumC(2))
      )
    ) {
      desugar(parse("(let ((x 1) (y 2)) (+ x y))"))
    }
  }


  test("Interp let") {
    assertResult(NumV(3)) {
      interp("(let ((x 1) (y 2)) (+ x y))")
    }
  }

  test("Interp let - factorial") {
    assertResult(NumV(24)) {
      interp("(let ((fact 0)) (seq (set fact (lambda (x) (if (num= x 0) 1 (* x (fact (- x 1)))))) (fact 4)))")
    }
  }

  test("Interp let - fibonacci") {
    assertResult(NumV(13)) {
      interp("(let ((fib 0)) (seq (set fib (lambda (x) (if (num< x 2) (if (num= x 0) 0 1) (+ 0 (+ (fib (- x 1)) (fib (- x 2))))))) (fib 7)))")
    }
  }


  test("Test scope") {
    assertResult(NumV(2)) {
      interp("((let ((x 1)) (lambda (y) (+ x 1))) 3)")
    }
  }

  test("Test scope - 2") {
    assertResult(NumV(8)) {
      interp("((let ((x 1)) (lambda (x) (+ x 3))) 5)")
    }
  }



  test("Let error") {
    intercept[ParseException] {
      interp("(let () (+ x 1))")
    }
  }

  test("Let error - 2") {
    intercept[ParseException] {
      parse("(let ((x)) (+ y 1))")
    }
  }

  test("Let error - 3") {
    intercept[InterpException] {
      interp("(let ((x y)) (+ x 3))")
    }
  }

  test("Let error - 4") {
    intercept[ParseException] {
      parse("(let ((x 1) (x 2)) (+ x 1))")
    }
  }

  test("Let error - 5") {
    intercept[ParseException] {
      interp("(let ((let 1) (nil 3)) (+ 3 2))")
    }
  }



  test("Set error") {
    assertResult(NumV(3)) {
      interp("(let ((x 0)) (set x 3))")
    }
  }




  //cons
  test("Test cons") {
    assertResult(ConsV(NumV(3), NilV())) {
      interp("(cons 3 nil)")
    }
  }

  test("Test cons desugar") {
    assertResult(ConsC(NumC(3), NilC())) {
      desugar(parse("(cons 3 nil)"))
    }
  }

  test("Test list") {
    assertResult(ListExt(List(NumExt(3), NumExt(5), NumExt(7)))) {
      parse("(list 3 5 7)")
    }
  }

  test("Test list desugar") {
    assertResult(ConsC(NumC(3), ConsC(NumC(5), ConsC(NumC(7), NilC())))) {
      desugar(parse("(list 3 5 7)"))
    }
  }





  //Lambda
  test("Parse lambda") {
    assertResult(
      FdExt(
        List("x", "y"),
        BinOpExt("*", IdExt("x"), IdExt("y"))
      )
    ) {
      parse("(lambda (x y) (* x y))")
    }
  }

  test("Parse lambda - 2") {
    assertResult(
      AppExt(
        FdExt(List("x"), BinOpExt("*", IdExt("x"), NumExt(2))),
        List(NumExt(3))
      )
    ) {
      parse("((lambda (x) (* x 2)) 3)")
    }
  }

  test("Parse lambda - 3") {
    assertResult(FdExt(Nil, BinOpExt("*", NumExt(3), NumExt(2)))) {
      parse("(lambda () (* 3 2))")
    }
  }

  test("Desugar lambda") {
    assertResult(
      FdC(
        List("x", "y"), MultC(IdC("x"), IdC("y"))
      )
    ) {
      desugar(parse("(lambda (x y) (* x y))"))
    }
  }

  test("Desugar lambda - 2") {
    assertResult(
      AppC(
        FdC(List("x"), MultC(IdC("x"), NumC(2))),
        List(NumC(3))
      )
    ) {
      desugar(parse("((lambda (x) (* x 2)) 3)"))
    }
  }

  test("Desugar lambda - 3") {
    assertResult(
      AppC(
        FdC(List("x", "y"), IfC(EqNumC(IdC("x"), NumC(3)), IdC("y"), NumC(1))),
        List(NumC(3), NumC(2))
      )
    ) {
      desugar(parse("((lambda (x y) (if (num= x 3) y 1)) 3 2)"))
    }
  }

  test("Desugar lambda - 4") {
    assertResult(
      AppC(
        FdC(List("x"), AppC(FdC(List("y"), PlusC(IdC("x"), IdC("y"))), List(NumC(2)))),
        List(NumC(3))
      )
    ) {
      desugar(parse("((lambda (x) ((lambda (y) (+ x y)) 2)) 3)"))
    }
  }

  test("Desugar lambda - 5") {
    assertResult(
      AppC(
        FdC(List("x"), FdC(List("x"), PlusC(IdC("x"), NumC(2)))),
        List(NumC(3))
      )
    ) {
      desugar(parse("((lambda (x) (lambda (x) (+ x 2))) 3)"))
    }
  }

  test("Interp lambda") {
    assertResult(NumV(6)) {
      interp("((lambda (x) (* x 2)) 3)")
    }
  }

  test("Interp lambda - 2") {
    assertResult(NumV(8)) {
      interp("((lambda (x y) (* x y)) 4 2)")
    }
  }

  test("Interp lambda - 3") {
    assertResult(NumV(1)) {
      interp("((lambda (x) (if (num= x 3) 2 1)) 5)")
    }
  }

  test("Interp lambda - 4") {
    assertResult(NumV(5)) {
      interp("((lambda (x) ((lambda (y) (+ x y)) 2)) 3)")
    }
  }

  test("Lambda error") {
    intercept[InterpException] {
      interp("((lambda (x) (* g 1)) 3)")
    }
  }

  test("Lambda error - 2") {
    intercept[ParseException] {
      parse("(lambda (1) (* x 1))")
    }
  }

  test("Lambda error - 3") {
    intercept[ParseException] {
      parse("(lambda (let nil) (+ 3 1))")
    }
  }

  test("Lambda error - 4") {
    intercept[InterpException] {
      interp("((lambda (x y) (* x y)) 4 3 2)")
    }
  }

  test("Lambda error - 5") {
    intercept[ParseException] {
      parse("(lambda (x x) (+ 3 1))")
    }
  }


  /**
  test("Parse lambda") {
    assertResult(
      FdExt(
        List("x", "y"),
        BinOpExt("*", IdExt("x"), IdExt("y"))
      )
    ) {
      parse("(lambda (x y) (* x y))")
    }
  }
    **/


  //Rec-lam
  test("Parse rec-lam") {
    assertResult(
      RecLamExt("f", "x", IfExt(
        BinOpExt("num=", IdExt("x"), NumExt(0)),
        NumExt(0),
        BinOpExt("+", IdExt("x"), NumExt(1))
      ))
    ) {
      parse("(rec-lam f (x) (if (num= x 0) 0 (+ x 1)))")
    }
  }

  test("Parse rec-lam - 2") {
    intercept[ParseException] {
      parse("(let ((g (rec-lam f (lambda (x) (f x))))) (g 0))")
    }
  }

  test("Interp rec-lam") {
    assertResult(NumV(6)) {
      interp("((rec-lam f (x) (if (num= x 0) 0 (+ x (f (- x 1))))) 3)")
    }
  }

  test("Rec-lam error - 1") {
    intercept[ParseException] {
      parse("(rec-lam f (nil) (+ 7 3))")
    }
  }

  test("Rec-lam error - 2") {
    intercept[ParseException] {
      parse("(rec-lam nil (x) (+ 7 3))")
    }
  }





  //Functions
  test("Parse function") {
    assertResult(
      LetExt(
        List(LetBindExt("f", FdExt(List("x"), BinOpExt("*", IdExt("x"), NumExt(1))))),
        AppExt(IdExt("f"), List(NumExt(3)))
      )
    ) {
      parse("(let ((f (lambda (x) (* x 1)))) (f 3))")
    }
  }

  test("Desugar function") {
    assertResult(
      AppC(
        FdC(List("f"), AppC(IdC("f"), List(NumC(3)))),
        List(FdC(List("x"), MultC(IdC("x"), NumC(1))))
      )
    ) {
      desugar(parse("(let ((f (lambda (x) (* x 1)))) (f 3))"))
    }
  }

  test("Interp function") {
    assertResult(NumV(3)) {
      interp("(let ((f (lambda (x) (* x 1)))) (f 3))")
    }
  }

  test("Interp error") {
    intercept[InterpException] {
      interp("(let ((f (lambda (x) (* x 1)))) (g 3))")
    }
  }



  //Nested function
  test("Interp fibonacci") {
    assertResult(NumV(13)) {
      interp("""((lambda (x)
                  (let ((f
                    (lambda (self x)
                      (if (num< x 2) (if (num= x 0) 0 1) (+ 0 (+ (self self (- x 1)) (self self (- x 2)))))
                    )
                  )) (f f x))
                ) 7)""")
    }
  }

  test("Interp counting") {
    assertResult(NumV(2)) {
      interp("""(let ((counter
                  (lambda ()
                    (let ((n (box 0)))
                      (list
                        (lambda () (setbox n (+ 1 (unbox n))))
                        (lambda () (setbox n 0))
                      )
                    )
                  )))
                  (let ((c (counter))) (seq ((head c)) ((head c))))
                )""")
    }
  }

  test("Self-ref box") {
    assertResult(NumV(1)) {
      interp("""(let ((b (box 0)))
                  (seq (setbox b b) (seq (unbox (unbox (unbox (unbox b)))) 1))
                )""")
    }
  }

  //(letrec ((sum (lambda (x) (if (num= x 0) 0 (+ x (sum (- x 1))))))) (sum 3))
  test("Self-ref func") {
    assertResult(NumV(6)) {
      interp("""(let ((sum 0))
                  (seq (set sum (lambda (x) (if (num= x 0) 0 (+ x (sum (- x 1))))))
                    (sum 3)
                  )
                )""")
    }
  }




  //nil
  test("Test nil") {
    assertResult(NilV()) {
      interp("nil")
    }
  }

  test("Test nil desugar") {
    assertResult(NilC()) {
      desugar(parse("nil"))
    }
  }

  //head
  test("Test head - one number") {
    assertResult(NumV(5)) {
      interp("(head (cons 5 (cons 6 1)))")
    }
  }

  //

  //tail
  test("Test tail - one number") {
    assertResult(NumV(6)) {
      interp("(tail (cons 5 6))")
    }
  }


  //is-list
  test("Test if list") {
    assertResult(BoolV(true)) {
      interp("(is-list (cons 5 6))")
    }
  }

  test("Test if list - false") {
    assertResult(BoolV(false)) {
      interp("(is-list 5)")
    }
  }

  test("Test is list") {
    assertResult(BoolV(true)) {
      interp("(is-list nil)")
    }
  }

  //is-nil
  test("Test if nil - true") {
    assertResult(BoolV(true)) {
      interp("(is-nil nil)")
    }
  }


  //Erroneous tests
  test("Catch erroneous parse behavior") {
    intercept[ParseException] {
      parse("()")
    }
  }

  test("Catch erroneous interp behavior") {
    intercept[InterpException] {
      interp("(+ true 5)")
    }
  }

  test("Catch head error") {
    intercept[InterpException] {
      interp("(head nil)")
    }
  }

  test("Catch tail error") {
    intercept[InterpException] {
      interp("(tail nil)")
    }
  }

  test("Catch is-nil error") {
    intercept[InterpException] {
      interp("(is-nil 5)")
    }
  }


  //Boxes
  test("Test box") {
    assertResult(NumV(10)) {
      interp("(let ((b (box 1))) (seq (setbox b 10) (unbox b)))")
    }
  }

  test("Test box - 2") {
    assertResult(NumV(20)) {
      interp("(let ((b (box 5)) (f (box 0))) (seq (setbox f (if (num> 20 5) 18 20)) (unbox f)))")
    }
  }



  //Letrec
  //(letrec ((x 1)) x)
  test("Test letrec - 1") {
    assertResult(NumV(1)) {
      interp(desugar(
        LetRecExt(
          List(LetBindExt("x", NumExt(1))),
          IdExt("x")
        )
      ))
    }
  }

  //(letrec ((x 1) (y 2)) (+ x y))
  test("Test letrec - 2") {
    assertResult(NumV(3)) {
      interp(desugar(
        LetRecExt(
          List(LetBindExt("x", NumExt(1)), LetBindExt("y", NumExt(2))),
          BinOpExt("+", IdExt("x"), IdExt("y"))
        )
      ))
    }
  }

  //(letrec ((x x)) x)
  test("Test letrec - 4") {
    assertResult(UninitializedV()) {
      interp(desugar(
        LetRecExt(
          List(LetBindExt("x", IdExt("x"))),
          IdExt("x")
        )
      ))
    }
  }

  test("Test letrec - fact") {
    assertResult(NumV(24)) {
      interp("""(letrec
                  ((fact
                    (lambda (n) (if (num= n 0) 1 (* n (fact (- n 1)))))
                  ))
                  (fact 4)
                )""")
    }
  }

  test("Test letrec - fib") {
    assertResult(NumV(8)) {
      interp("""(letrec
                  ((fib
                    (lambda (n) (if (num= n 0) 0 (if (num= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))
                  ))
                  (fib 6)
                )""")
    }
  }

  test("Test letrec - sum") {
    assertResult(NumV(10)) {
      interp("""(letrec
                  ((sum
                    (lambda (n) (if (num= n 0) 0 (+ n (sum (- n 1)))))
                  ))
                  (sum 4)
                )""")
    }
  }

  test("Test letrec - 5") {
    assertResult(NumV(13)) {
      interp("""(let ((a 0) (b 1) (sum 0))
                  (letrec
                    ((fib
                      (lambda (n)
                        (if (or (num= n 0) (num= n 1))
                          sum
                          (seq (set sum (+ a b))
                            (seq (set a b)
                              (seq (set b sum)
                                (fib (- n 1))
                              )
                            )
                          )
                        )
                      )
                    ))
                    (fib 7)
                  )
                )""")
    }
  }


  def interp(expr: String): Value = Interp.interp(desugar(parse(expr)), Nil, Nil)._1
  def interp(expr: ExprC): Value = Interp.interp(expr, Nil, Nil)._1
  def interp(expr: ExprC, nv: PointerEnvironment, st: Store): (Value, Store) = Interp.interp(expr, nv, st)
}
