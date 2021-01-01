package test

import org.scalatest.FunSuite
import Parser._
import Library._

class ParserTest extends FunSuite {
  test("parse test") {
    assertResult(
      PlusC(NumC(23), MultC(NumC(5), NumC(6)))
    ) {
      parse("(+ 23 (* 5 6))")
      //SList(List(SSym("+"), SNum(23), SList(List(SSym("*"), SNum(5), SNum(6)))))
    }
  }

  test("parse-test-another") {
    assertResult(
      MultC(MultC(NumC(2), NumC(10)), PlusC(NumC(12), NumC(20)))
    ) {
      parse("(* (* 2 10) (+ 12 20))")
    }
  }
}
