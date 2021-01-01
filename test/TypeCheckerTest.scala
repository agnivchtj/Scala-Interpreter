package test

//test: Test
import Parser._
import Desugar._
import Interp._
import TypeChecker._
import Typed._

import org.scalatest.FunSuite

class TypeCheckerTest extends FunSuite with CatchErrorSuite {

  /**
    * Tests for Parsing
    */

  test("Parse 5") {
    assertResult(
      NumExt(5)
    ) {
      parse("5")
    }
  }

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

  //pair 1 true
  test("Desugar pairs - 1") {
    assertResult(PairC(NumC(1), TrueC())) {
      desugar(BinOpExt("pair", NumExt(1), TrueExt()))
    }
  }

  //pair (pair 1 true) (nil : Num)
  test("Desugar pairs - 2") {
    assertResult(PairC(PairC(NumC(1), TrueC()), NilC())) {
      desugar(BinOpExt("pair", BinOpExt("pair", NumExt(1), TrueExt()), NilExt(NumT())))
    }
  }

  //fst (pair 1 true)
  test("Desugar pairs - 3") {
    assertResult(FstC(PairC(NumC(1), TrueC()))) {
      desugar(UnOpExt("fst", BinOpExt("pair", NumExt(1), TrueExt())))
    }
  }

  //snd (pair 1 true)
  test("Desugar pairs - 4") {
    assertResult(SndC(PairC(NumC(1), TrueC()))) {
      desugar(UnOpExt("snd", BinOpExt("pair", NumExt(1), TrueExt())))
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
    * Tests for Type Checking
    */

  test("Type Check 5") {
    assertResult(NumT()) {
      typeOf(NumExt(5), Nil)
    }
  }



  //(tail (cons 1 (nil : Num)))
  test("Typeof tail+cons") {
    assertResult(ListT(NumT())) {
      typeOf(UnOpExt("tail", BinOpExt("cons", NumExt(1), NilExt(NumT()))))
    }
  }

  //cons 1 nil => (List Num)
  test("Cons test - 1") {
    assertResult(ListT(NumT())) {
      typeOf(BinOpExt("cons", NumExt(1), NilExt(NumT())))
    }
  }

  //cons 1 (cons 2 nil) => (List Num)
  test("Cons test - 2") {
    assertResult(ListT(NumT())) {
      typeOf(BinOpExt("cons", NumExt(1), BinOpExt("cons", NumExt(2), NilExt(NumT()))))
    }
  }

  //cons true (cons false nil) => (List Bool)
  test("Cons test - 3") {
    assertResult(ListT(BoolT())) {
      typeOf(BinOpExt("cons", TrueExt(), BinOpExt("cons", FalseExt(), NilExt(BoolT()))))
    }
  }

  //cons (pair 1 true) nil => (List (Pair Num Bool))
  test("Cons test --> 3a") {
    assertResult(ListT(PairT(NumT(), BoolT()))) {
      typeOf(BinOpExt("cons", BinOpExt("pair", NumExt(1), TrueExt()), NilExt(PairT(NumT(), BoolT()))))
    }
  }

  //cons (cons 1 nil) nil => (List (List Num))
  test("Cons test - 4") {
    assertResult(ListT(ListT(NumT()))) {
      typeOf(BinOpExt("cons", BinOpExt("cons", NumExt(1), NilExt(NumT())), NilExt(ListT(NumT()))))
    }
  }

  //cons (cons (cons 1 nil) nil) nil => (List (List (List Num)))
  test("Cons test --> 4a") {
    assertResult(ListT(ListT(ListT(NumT())))) {
      typeOf(BinOpExt(
        "cons",
        BinOpExt(
          "cons",
          BinOpExt(
            "cons",
            NumExt(1),
            NilExt(NumT())
          ),
          NilExt(ListT(NumT()))
        ),
        NilExt(ListT(ListT(NumT())))
      ))
    }
  }

  //cons (cons true nil) nil => (List (List Bool))
  test("Cons test - 5") {
    assertResult(ListT(ListT(BoolT()))) {
      typeOf(BinOpExt("cons", BinOpExt("cons", TrueExt(), NilExt(BoolT())), NilExt(ListT(BoolT()))))
    }
  }

  //cons true (cons 1 nil) => MISMATCH
  test("Cons test - 6") {
    intercept[TypeException] {
      typeOf(BinOpExt("cons", TrueExt(), BinOpExt("cons", NumExt(1), NilExt(NumT()))))
    }
  }




  //(list 1 2 3) => (List Num)
  test("List test - 1") {
    assertResult(ListT(NumT())) {
      typeOf(ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3))))
    }
  }

  //(list true false true) => (List Bool)
  test("List test - 2") {
    assertResult(ListT(BoolT())) {
      typeOf(ListExt(BoolT(), List(TrueExt(), FalseExt(), TrueExt())))
    }
  }

  //(list 1 true 3) => exception
  test("List test - 3") {
    intercept[TypeException] {
      typeOf(ListExt(NumT(), List(NumExt(1), TrueExt(), NumExt(3))))
    }
  }

  //(list (pair 1 true) (pair 3 false)) => (List (Pair Num Bool))
  test("List test - 4") {
    assertResult(ListT(PairT(NumT(), BoolT()))) {
      typeOf(
        ListExt(PairT(NumT(), BoolT()),
          List(BinOpExt("pair", NumExt(1), TrueExt()), BinOpExt("pair", NumExt(3), FalseExt())))
      )
    }
  }

  //(list : Num ())
  test("List test - 5") {
    assertResult(ListT(NumT())) {
      typeOf(ListExt(NumT(), Nil))
    }
  }



  //is-nil (list 1 2 3) => Bool
  test("Is-nil test - 1") {
    assertResult(BoolT()) {
      typeOf(parse("(is-nil (list : Num (1 2 3)))"))
    }
  }

  //is-nil (cons 1 (cons 2 nil)) => Bool
  test("is-nil test - 2") {
    assertResult(BoolT()) {
      typeOf(parse("(is-nil (cons 1 (cons 2 (nil : Num))))"))
    }
  }

  //is-nil nil => Bool
  test("is-nil test - 3") {
    assertResult(BoolT()) {
      typeOf(parse("(is-nil (nil : Num))"))
    }
  }

  //is-nil 3 => exception
  test("is-nil test - 4") {
    intercept[TypeException] {
      typeOf(parse("(is-nil 3)"))
    }
  }

  //is-nil (list 1 true 3)
  test("is-nil test - 5") {
    intercept[TypeException] {
      typeOf(parse("(is-nil (list : Num (1 true 3)))"))
    }
  }

  //
  test("is-nil test - 6") {
    intercept[TypeException] {
      typeOf(parse("(is-nil (cons 1 (cons 2 (nil : Bool))))"))
    }
  }


  //head (list 1 2 3) => Num
  test("Head list") {
    assertResult(NumT()) {
      typeOf(UnOpExt("head", ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3)))))
    }
  }

  //tail (list 1 2 3) => (List Num)
  test("Tail list") {
    assertResult(ListT(NumT())) {
      typeOf(UnOpExt("tail", ListExt(NumT(), List(NumExt(1), NumExt(2), NumExt(3)))))
    }
  }




  //pair 1 true => (Pair Num Bool)
  test("Pair test - 1") {
    assertResult(PairT(NumT(), BoolT())) {
      typeOf(BinOpExt("pair", NumExt(1), TrueExt()))
    }
  }

  //pair (pair 1 true) (nil : Num) => (Pair (Pair Num Bool) (List Num))
  test("Pair test - 2") {
    assertResult(PairT(PairT(NumT(), BoolT()), ListT(NumT()))) {
      typeOf(BinOpExt("pair", BinOpExt("pair", NumExt(1), TrueExt()), NilExt(NumT())))
    }
  }



  //fst (pair 1 true) => Num
  test("Fst test") {
    assertResult(NumT()) {
      typeOf(UnOpExt("fst", BinOpExt("pair", NumExt(1), TrueExt())))
    }
  }

  //snd (pair 1 true) => Bool
  test("Snd test") {
    assertResult(BoolT()) {
      typeOf(UnOpExt("snd", BinOpExt("pair", NumExt(1), TrueExt())))
    }
  }




  //
  //RecLamExt(name: String, paramTy: Type, retTy: Type, param: String, body: ExprExt)
  test("Test reclam - 1") {
    assertResult(FunT(NumT() :: Nil, NumT())) {
      typeOf(parse("(rec-lam (f : Num -> Num) (x) (if (num= x 0) 0 (+ x 1)))"))
    }
  }

  test("Test reclam - 2") {
    assertResult(FunT(NumT() :: Nil, NumT())) {
      typeOf(parse("(rec-lam (f : Num -> Num) (x) (if (num= x 0) 0 (+ x (f (- x 1)))))"))
    }
  }

  test("Test reclam - 3") {
    assertResult(FunT(NumT() :: Nil, NumT())) {
      typeOf(parse("(rec-lam (f : Num -> Num) (x) (if (num= x 0) 1 (* x (f (- x 1)))))"))
    }
  }

  test("Test reclam - 4") {
    assertResult(FunT(NumT() :: Nil, NumT())) {
      typeOf(parse("(rec-lam (f : Num -> Num) (x) (if (num< x 2) (if (num= x 0) 0 1) (+ 0 (+ (f (- x 1)) (f (- x 2))))))"))
    }
  }



  //(letrec (((x : Num) 1)) x)
  test("Test letrec - 1") {
    assertResult(NumT()) {
      typeOf(
        LetRecExt(
          List(LetRecBindExt("x", NumT(), NumExt(1))),
          IdExt("x")
        )
      )
    }
  }

  //(letrec (((x : Num) 1) ((y : Num) 2)) (+ x y))
  test("Test letrec - 2") {
    assertResult(NumT()) {
      typeOf(
        LetRecExt(
          List(LetRecBindExt("x", NumT(), NumExt(1)), LetRecBindExt("y", NumT(), NumExt(2))),
          BinOpExt("+", IdExt("x"), IdExt("y"))
        )
      )
    }
  }

  //(letrec (((x : Bool) 1)) x)
  test("Test letrec - 3") {
    intercept[TypeException] {
      typeOf(
        LetRecExt(
          List(LetRecBindExt("x", BoolT(), NumExt(1))),
          IdExt("x")
        )
      )
    }
  }

  //(letrec (((x : Num) x)) x)
  test("Test letrec - 4") {
    assertResult(NumT()) {
      typeOf(
        LetRecExt(
          List(LetRecBindExt("x", NumT(), IdExt("x"))),
          IdExt("x")
        )
      )
    }
  }



  //Lambda
  //(lambda ((x : Num)) (+ x 1))
  test("TypeOf lambda - 1") {
    assertResult(FunT(List(NumT()), NumT())) {
      typeOf(
        FdExt(
          List(Param("x", NumT())),
          BinOpExt("+", IdExt("x"), NumExt(1))
        )
      )
    }
  }

  //((lambda ((x : Num)) x) 1)
  test("TypeOf lambda - 2") {
    assertResult(NumT()) {
      typeOf(
        AppExt(
          FdExt(
            List(Param("x", NumT())),
            IdExt("x")
          ),
          List(NumExt(1))
        )
      )
    }
  }

  //(lambda ((x : Num) (y : Num)) (+ x y))
  test("TypeOf lambda - 3") {
    assertResult(FunT(List(NumT(), NumT()), NumT())) {
      typeOf(
        FdExt(
          List(Param("x", NumT()), Param("y", NumT())),
          BinOpExt("+", IdExt("x"), IdExt("y"))
        )
      )
    }
  }




  //LetBindExt(name: String, value: ExprExt)
  //(let ((x 1) (y 2)) (+ x y))
  test("typeOf let - 1") {
    assertResult(NumT()) {
      typeOf(
        LetExt(
          List(LetBindExt("x", NumExt(1)), LetBindExt("y", NumExt(2))),
          BinOpExt("+", IdExt("x"), IdExt("y"))
        )
      )
    }
  }





  //(let ((b (box 1))) (seq (setbox b 10) (unbox b)))
  test("Test let+box") {
    assertResult(NumT()) {
      typeOf(
        LetExt(
          List(LetBindExt("b", UnOpExt("box", NumExt(1)))),
          BinOpExt("seq", BinOpExt("setbox", IdExt("b"), NumExt(10)), UnOpExt("unbox", IdExt("b")))
        )
      )
    }
  }

  test("Test box") {
    assertResult(NumT()) {
      typeOf(parse("(let ((b (box 1))) (seq (setbox b 10) (+ (unbox b) 1)))"))
    }
  }


  //(let ((b (box 5)) (f (box 0))) (seq (setbox f (if (num> 20 5) 18 20)) (unbox f)))
  test("Test let+box - 2") {
    assertResult(NumT()) {
      typeOf(
        LetExt(
          List(LetBindExt("b", UnOpExt("box", NumExt(5))), LetBindExt("f", UnOpExt("box", NumExt(0)))),
          BinOpExt(
            "seq",
            BinOpExt("setbox", IdExt("f"), IfExt(BinOpExt("num>", NumExt(20), NumExt(5)), NumExt(18), NumExt(5))),
            UnOpExt("unbox", IdExt("f"))
          )
        )
      )
    }
  }

  //(let ((x 0)) (seq (set x 42) x))
  //LetExt(binds: List[LetBindExt], body: ExprExt)
  test("Test set") {
    assertResult(NumT()) {
      typeOf(
        LetExt(
          List(LetBindExt("x", NumExt(0))),
          BinOpExt("seq", SetExt("x", NumExt(42)), IdExt("x"))
        )
      )
    }
  }

  test("Test set - 2") {
    assertResult(NumT()) {
      typeOf(parse("(let ((x 0)) (seq (set x 42) (* x 2)))"))
    }
  }




  //(box 1)
  test("Box test") {
    assertResult(RefT(NumT())) {
      typeOf(UnOpExt("box", NumExt(1)))
    }
  }

  //(unbox (box 1))
  test("Box test - 2") {
    assertResult(NumT()) {
      typeOf(UnOpExt("unbox", UnOpExt("box", NumExt(1))))
    }
  }

  //(unbox (unbox (unbox (box (box 1)))))
  test("Box test - 3") {
    intercept[TypeException] {
      typeOf(UnOpExt("unbox", UnOpExt("unbox", UnOpExt("unbox", UnOpExt("box", UnOpExt("box", NumExt(1)))))))
    }
  }

  //(box (box 1))
  test("Box test - 4") {
    assertResult(RefT(RefT(NumT()))) {
      typeOf(UnOpExt("box", UnOpExt("box", NumExt(1))))
    }
  }


  //(setbox (box 1) 2)
  test("Box test - 5") {
    assertResult(NumT()) {
      typeOf(BinOpExt("setbox", UnOpExt("box", NumExt(1)), NumExt(2)))
    }
  }

  //(setbox (box 1) true)
  test("Box test - 6") {
    intercept[TypeException] {
      typeOf(BinOpExt("setbox", UnOpExt("box", NumExt(1)), TrueExt()))
    }
  }




  /**
    * Tests for Safe Interpretation
    */
  test("Safe Interp 5") {
    assertResult(
      NumV(5)
    ) {
      SafeInterp.interp(parse("5"))
    }
  }

  test("Safe Interp (+ 5 true)") {
    intercept[TypeException] {
      SafeInterp.interp(parse("(+ 5 true)"))
    }
  }




  /**
    * Helpers
    */
  def typeOf(e: String): Type = TypeChecker.typeOf(parse(e))
  def typeOf(e: String, nv: List[TBind]): Type = TypeChecker.typeOf(parse(e), nv)
  def typeOf(e: ExprExt): Type = TypeChecker.typeOf(e)
  def typeOf(e: ExprExt, nv: List[TBind]): Type = TypeChecker.typeOf(e, nv)
}
