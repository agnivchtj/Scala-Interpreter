package test

//test: Test
import Untyped._
import Parser._
import Desugar._
import Interp._

import org.scalatest.FunSuite

class InterpBasicTest extends FunSuite with CatchErrorSuite {

  /**
    * Tests for Parsing
    */
  test("Parse 5") {
    assertResult(NumExt(5)) {
      parse("5")
    }
  }

  test("Interp 5") {
    assertResult(NumV(5)) {
      interp(desugar(parse("5")))
    }
  }

  test("Verify true") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("true")))
    }
  }

  test("Test list") {
    assertResult(ListExt(List(NumExt(3), NumExt(5), NumExt(7)))) {
      parse("(list 3 5 7)")
    }
  }

  /**
    * Tests for Desugaring
    */
  test("Desugar 5") {
    assertResult(NumC(5)) {
      desugar(NumExt(5))
    }
  }

  test("Test list desugar") {
    assertResult(ConsC(NumC(3), ConsC(NumC(5), ConsC(NumC(7), NilC())))) {
      desugar(parse("(list 3 5 7)"))
    }
  }


  test("Test nil desugar") {
    assertResult(NilC()) {
      desugar(parse("nil"))
    }
  }

  /**
    * Tests for Interpreting
    */
  test("Test cons") {
    assertResult(ConsV(NumV(3), NilV())) {
      interp(desugar(parse("(cons 3 nil)")))
    }
  }

  test("Test cons desugar") {
    assertResult(ConsC(NumC(3), NilC())) {
      desugar(parse("(cons 3 nil)"))
    }
  }


  test("Test nil") {
    assertResult(NilV()) {
      interp(desugar(parse("nil")))
    }
  }


  test("Test head - one number") {
    assertResult(NumV(5)) {
      interp(desugar(parse("(head (cons 5 (cons 6 1)))")))
    }
  }

  test("Test tail - one number") {
    assertResult(NumV(6)) {
      interp(desugar(parse("(tail (cons 5 6))")))
    }
  }

  //
  //Conditional
  test("Test cond - 1") {
    assertResult(NumV(1)) {
      interp(desugar(parse("(cond ((num< 1 0) 0) (else 1))")))
    }
  }

  test("Test cond - 2") {
    assertResult(NumV(3)) {
      interp(desugar(parse("(cond (false 1) (true 3))")))
    }
  }

  test("Catch cond exception") {
    intercept[InterpException] {
      interp(desugar(parse("(cond ((num> 0 1) 1) ((num< 1 0) 2))")))
    }
  }



  test("Parse cond - 1") {
    assertResult(
      CondEExt(
        List(
          (FalseExt(), NumExt(0)),
          (FalseExt(), NumExt(3))
        ), NumExt(2)
      )
    ) {
      parse("(cond (false 0) (false 3) (else 2))")
    }
  }

  test("Parse cond - 2") {
    assertResult(
      CondEExt(
        List(
          (BinOpExt("num<", NumExt(1), NumExt(0)), NumExt(0))
        ), NumExt(1)
      )
    ) {
      parse("(cond ((num< 1 0) 0) (else 1))")
    }
  }

  test("Parse cond - 3") {
    assertResult(
      CondExt(
        List(
          (BinOpExt("num<", NumExt(1), NumExt(2)), NumExt(0)),
          (BinOpExt("num=", NumExt(3), NumExt(0)), NumExt(1))
        )
      )
    ) {
      parse("(cond ((num< 1 2) 0) ((num= 3 0) 1))")
    }
  }

  test("Parse cond - 4") {
    assertResult(
      CondEExt(
        List(
          (BinOpExt("num<", NumExt(1), NumExt(0)), NumExt(0)),
          (BinOpExt("num=", NumExt(3), NumExt(0)), NumExt(1))
        ), NumExt(1)
      )
    ) {
      parse("(cond ((num< 1 0) 0) ((num= 3 0) 1) (else 1))")
    }
  }

  test("Parse cond exception") {
    intercept[ParseException] {
      parse("(cond ())")
    }
  }

  test("Parse cond excep -2") {
    intercept[ParseException] {
      parse("(cond (true 1 2))")
    }
  }

  test("Parse cond excep - 3") {
    intercept[ParseException] {
      parse("(cond (else 3))")
    }
  }





  test("Test if list") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-list (cons 5 6))")))
    }
  }

  test("Test if list - false") {
    assertResult(BoolV(false)) {
      interp(desugar(parse("(is-list 5)")))
    }
  }

  test("Test is list") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-list nil)")))
    }
  }

  //is-nil
  test("Test if nil - true") {
    assertResult(BoolV(true)) {
      interp(desugar(parse("(is-nil nil)")))
    }
  }


  //Erroneous tests
  test("Interp (5 + true) throws InterpException") {
    intercept[InterpException] {
      interp(PlusC(NumC(5), TrueC()))
    }
  }

  test("Catch erroneous parse behavior") {
    intercept[ParseException] {
      parse("()")
    }
  }

  test("Catch failing parse behaviour") {
    intercept[ParseException] {
      interp(desugar(parse("(+ 3)")))
    }
  }

  test("Catch erroneous interp behavior") {
    intercept[InterpException] {
      interp(desugar(parse("(+ true 5)")))
    }
  }

  test("Catch head error") {
    intercept[InterpException] {
      interp(desugar(parse("(head nil)")))
    }
  }

  test("Catch tail error") {
    intercept[InterpException] {
      interp(desugar(parse("(tail nil)")))
    }
  }

  test("Catch is-nil error") {
    intercept[InterpException] {
      interp(desugar(parse("(is-nil 5)")))
    }
  }

}
