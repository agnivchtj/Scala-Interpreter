package Interp_Basic

object Exercise {
  sealed abstract class SExpr
  case class SList(list: List[SExpr]) extends SExpr
  case class SSym(symbol: String) extends SExpr
  case class SNum(num: Int) extends SExpr
  case class SString(string: String) extends SExpr

  /**
    * Write an s-expression whose result is 42.
    */
  def is42 = {
    "42"
  }

  /**
    * Write an s-expression that evaluates to a list containing the numbers
    * between and including 0 and 3.
    */
  def between0and3 = {
    "(cons 0 (cons 1 (cons 2 (cons 3 nil))))"
  }
}
