package Intro

import Library._

// to do: define the other case classes of the intro.ArithC type
sealed abstract class ArithC
case class NumC(num: Int) extends ArithC
case class PlusC(l: ArithC, r: ArithC) extends ArithC
case class MultC(l: ArithC, r: ArithC) extends ArithC
case class ParseException(string: String) extends RuntimeException

object Parser {
  def parse(str: String): ArithC = parse(Reader.read(str))

  def parse(sexpr: SExpr): ArithC = {
    sexpr match {
      case SNum(num) => NumC(num)
      case SList(SSym("+") :: y :: z :: tail) => PlusC(parse(y), parse(z))
      case SList(SSym("*") :: y :: z :: tail) => MultC(parse(y), parse(z))
      case _ => throw new RuntimeException("Invalid input")
    }
  }
}
