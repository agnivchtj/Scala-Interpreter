package test

//test: Test
import org.scalatest.FunSuite
import Parser._
import Desugar._
import Interp._
import Untyped._

class Func_EnvTest extends FunSuite with CatchErrorSuite {

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
      interp(NumC(5), List())
    }
  }

  test("Interp 5+true throws InterpException") {
    intercept[InterpException] {
      interp(PlusC(NumC(5), TrueC()), Nil)
    }
  }

  test("Verify correct implementation") {
    assertResult(NumV(5)) {
      interp("5")
    }
  }

  test("Verify true") {
    assertResult(BoolV(true)) {
      interp("true")
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



  //Let
  test("Interp let") {
    assertResult(NumV(3)) {
      interp("(let ((x 1) (y 2)) (+ x y))")
    }
  }

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

  test("Let shadow - simple") {
    assertResult(NumV(2)) {
      interp("(let ((x 2)) x)")
    }
  }

  test("Let shadow - 2") {
    assertResult(NumV(2)) {
      interp("(let ((x 2)) (let ((x x)) x))")
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
    assertResult(
      AppExt(
        RecLamExt("f", "x", IfExt(
          BinOpExt("num=", IdExt("x"), NumExt(0)),
          NumExt(0),
          BinOpExt("+", IdExt("x"), NumExt(1))
        )),
        List(NumExt(3))
      )
    ) {
      parse("((rec-lam f (x) (if (num= x 0) 0 (+ x 1))) 3)")
    }
  }

  test("Parse rec-lam - 3") {
    intercept[ParseException] {
      parse("(let ((g (rec-lam f (lambda (x) (f x))))) (g 0))")
    }
  }

  test("Interp rec-lam") {
    assertResult(NumV(6)) {
      interp("((rec-lam f (x) (if (num= x 0) 0 (+ x (f (- x 1))))) 3)")
    }
  }

  test("Interp rec-lam - 2") {
    assertResult(NumV(4)) {
      interp("((rec-lam f (x) (if (num= x 0) 0 (+ x 1))) 3)")
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
      interp("((lambda (x) (let ((f (lambda (self x) (if (num< x 2) (if (num= x 0) 0 1) (+ 0 (+ (self self (- x 1)) (self self (- x 2)))))))) (f f x))) 7)")
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



  /**
    * Helpers
    */
  def interp(expr: String): Value = Interp.interp(desugar(parse(expr)), Nil)
  def interp(expr: ExprC, nv: Environment): Value = Interp.interp(expr, nv)

}
