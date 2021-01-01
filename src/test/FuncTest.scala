package test

//test: Test
import Parser._
import Desugar._
import Interp._
import Untyped._

import org.scalatest.FunSuite

class FuncTest extends FunSuite with CatchErrorSuite {

  //Let
  test("Interp let") {
    assertResult(NumV(3)) {
      interp(desugar(parse("(let ((x 1) (y 2)) (+ x y))")))
    }
  }

  //LetExt(binds: List[LetBindExt], body: ExprExt)
  //LetBindExt(name: String, value: ExprExt)
  test("Parse let") {
    assertResult(
      LetExt(
        List(LetBindExt("x", NumExt(1))),
        BinOpExt("+", IdExt("x"), NumExt(1))
      )
    ) {
      parse("(let ((x 1)) (+ x 1))")
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
    intercept[InterpException] {
      interp(desugar(parse("(let ((x 1)) (+ y 1))")))
    }
  }

  test("Let error - 2") {
    intercept[InterpException] {
      interp(desugar(parse("(let ((x x)) (+ 5 3))")))
    }
  }

  test("Let error - 3") {
    intercept[ParseException] {
      parse("(let x)")
    }
  }

  test("Let error - 4") {
    intercept[ParseException] {
      interp(desugar(parse("(let () (+ x 1))")))
    }
  }

  test("Let error - 5") {
    intercept[ParseException] {
      parse("(let ((x)) (+ y 1))")
    }
  }

  test("Let error - 6") {
    intercept[ParseException] {
      parse("(let )")
    }
  }

  test("Let error - 7") {
    intercept[ParseException] {
      parse("(let ((x 1) (x 2)) (+ x 3))")
    }
  }

  test("Let error - 8") {
    intercept[InterpException] {
      interp(desugar(parse("(let ((x 1) (y x)) (+ x y))")))
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
        FdC(List("x", "y"), MultC(IdC("x"), IdC("y"))),
        List(NumC(3), NumC(2))
      )
    ) {
      desugar(parse("((lambda (x y) (* x y)) 3 2)"))
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

  test("Interp lambda - 2") {
    assertResult(NumV(8)) {
      interp(desugar(parse("((lambda (x y) (* x y)) 4 2)")))
    }
  }

  test("Interp lambda") {
    assertResult(NumV(6)) {
      interp(desugar(parse("((lambda (x) (* x 2)) 3)")))
    }
  }

  test("Interp lambda - 3") {
    assertResult(NumV(1)) {
      interp(desugar(parse("((lambda (x) (if (num= x 3) 2 1)) 5)")))
    }
  }

  test("Interp lambda - 4") {
    assertResult(NumV(5)) {
      interp(desugar(parse("((lambda (x) ((lambda (y) (+ x y)) 2)) 3)")))
    }
  }

  test("Interp lambda - 5") {
    assertResult(NumV(12)) {
      interp(desugar(parse("((lambda (x y z) ((lambda (y) (+ x 1)) 3)) 11 2 1)")))
    }
  }

  test("Lambda error") {
    intercept[InterpException] {
      interp(desugar(parse("((lambda (x) (* x y)) 5)")))
    }
  }

  test("Lambda error - 2") {
    intercept[ParseException] {
      parse("(lambda () ())")
    }
  }

  test("Lambda error - 3") {
    intercept[ParseException] {
      parse("(lambda (x x) (+ x 1))")
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
      interp(desugar(parse("(let ((f (lambda (x) (* x 1)))) (f 3))")))
    }
  }

  test("Interp function - bad") {
    intercept[InterpException] {
      interp(desugar(parse("(let ((f (lambda (x) (* x 1)))) (g 3))")))
    }
  }


  //Nested functions
  test("Interp factorial") {
    assertResult(NumV(120)) {
      interp(desugar(parse(
        "((lambda (x) (let ((f (lambda (self x) (if (num= x 0) 1 (* x (self self (- x 1))))))) (f f x))) 5)"
      )))
    }
  }

  test("Interp fibonacci") {
    assertResult(NumV(13)) {
      interp(desugar(parse(
        "((lambda (x) (let ((f (lambda (self x) (if (num< x 2) (if (num= x 0) 0 1) (+ 0 (+ (self self (- x 1)) (self self (- x 2)))))))) (f f x))) 7)"
      )))
    }
  }

}
