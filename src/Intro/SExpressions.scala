package Intro

sealed abstract class SExpr
case class SList(list: List[SExpr]) extends SExpr
case class SSym(symbol: String) extends SExpr
case class SNum(num: Int) extends SExpr
case class SString(string: String) extends SExpr

// define the case classes for intro.SExpr

import scala.util.parsing.combinator._

object SExpressions extends JavaTokenParsers {
  def read(text: String): SExpr = {
    val result = parseAll(sexpr,text)

    result match {
      case Success(r,_) => r
      case Failure(msg, n) =>
        sys.error(msg + " (input left: \""+n.source.toString.drop(n.offset)+"\")")
      case Error(msg, n) =>
        sys.error(msg + " (input left: \""+n.source.toString.drop(n.offset)+"\")")
    }
  }

  def sexpr  : Parser[SExpr] = (num | symbol | slist)
  def symbol : Parser[SExpr] = not(wholeNumber) ~> "[^()\\s]+".r ^^ SSym
  def slist  : Parser[SExpr] = "(" ~> sexpr.+ <~ ")"          ^^ SList
  def num    : Parser[SExpr] = wholeNumber                    ^^ {s => SNum(s.toInt)}
}
