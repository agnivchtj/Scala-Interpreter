package Interp_Basic

import Library._
import Untyped._

case class PException(s: String) extends ParseException()
case class DException(s: String) extends DesugarException()
case class IException(s: String) extends InterpException()

object Parser {
  def parse(str: String): ExprExt = parse(Reader.read(str))

  def parse(sexpr: SExpr): ExprExt = {
    sexpr match {
      case SNum(num) => NumExt(num)

      case SSym("nil") => NilExt()
      case SSym("true") => TrueExt()
      case SSym("false") => FalseExt()

      //Binary operations
      case SList(SSym("+") :: x :: y :: Nil) => BinOpExt("+", parse(x), parse(y))
      case SList(SSym("*") :: x :: y :: Nil) => BinOpExt("*", parse(x), parse(y))
      case SList(SSym("-") :: x :: y :: Nil) => BinOpExt("-", parse(x), parse(y))
      case SList(SSym("and") :: x :: y :: Nil) => BinOpExt("and", parse(x), parse(y))
      case SList(SSym("or") :: x :: y :: Nil) => BinOpExt("or", parse(x), parse(y))
      case SList(SSym("num=") :: x :: y :: Nil) => BinOpExt("num=", parse(x), parse(y))
      case SList(SSym("num<") :: x :: y :: Nil) => BinOpExt("num<", parse(x), parse(y))
      case SList(SSym("num>") :: x :: y :: Nil) => BinOpExt("num>", parse(x), parse(y))
      case SList(SSym("cons") :: x :: y :: Nil) => BinOpExt("cons", parse(x), parse(y))

      //Uniary operations
      case SList(SSym("-") :: x :: Nil) => UnOpExt("-", parse(x))
      case SList(SSym("not") :: x :: Nil) => UnOpExt("not", parse(x))
      case SList(SSym("head") :: x :: Nil) => UnOpExt("head", parse(x))
      case SList(SSym("tail") :: x :: Nil) => UnOpExt("tail", parse(x))
      case SList(SSym("is-nil") :: x :: Nil) => UnOpExt("is-nil", parse(x))
      case SList(SSym("is-list") :: x :: Nil) => UnOpExt("is-list", parse(x))

      //Conditionals
      case SList(SSym("if") :: x :: y :: z :: Nil) => IfExt(parse(x), parse(y), parse(z))


      //
      //Is branches empty?
      //Is the last branch an else?
      //Do branches have more than just an else?
      case SList(SSym("cond") :: branches) =>
        /*
        println(checkBranches(branches))
        println(branches)
        println(getLast(branches))
        */
        //if (branches == Nil) throw new Interp_Basic.Func_Subst.Mutation.PException("Empty branches")
        if (checkBranches(branches) == false) CondExt(
          branches.map(i => i match {
            case SList(List(cond, exp)) => (parse(cond), parse(exp))
            case _ => throw new PException("Invalid")
          })
        )
        else if (checkBranches(branches) == true && branches.size > 1) CondEExt(
          branches.slice(0, branches.size - 1).map(i => i match {
            case SList(List(cond, exp)) => (parse(cond), parse(exp))
            case _ => throw new PException("Invalid")
          }),
          getLast(branches)
        )
        else throw new PException("Invalid")

      //List
      case SList(SSym("list") :: tail) => ListExt(tail.map(x => parse(x)))

      case _ => throw new PException("Invalid input")
    }
  }

  def checkBranches(s: List[SExpr]): Boolean = {
    s match {
      case SList(SSym("else") :: exp :: Nil) :: Nil => true
      case SList(cond :: exp :: Nil) :: Nil => false
      case SList(cond :: exp :: Nil) :: tail => checkBranches(tail)
      case _ => throw new PException("Invalid")
    }
  }

  def getLast(s: List[SExpr]): ExprExt = {
    s match {
      case SList(SSym("else") :: exp :: Nil) :: Nil => parse(exp)
      case SList(cond :: exp :: Nil) :: tail => getLast(tail)
      case _ => throw new PException("Invalid")
    }
  }
}

object Desugar {
  def desugar(e: ExprExt): ExprC = {
    e match {
      case NumExt(num) => NumC(num)
      case TrueExt() => TrueC()
      case FalseExt() => FalseC()

      case BinOpExt(s, l, r) => {
        s match {
          case "+" => PlusC(desugar(l), desugar(r))
          case "*" => MultC(desugar(l), desugar(r))
          case "-" => PlusC(desugar(l), MultC(desugar(NumExt(-1)), desugar(r)))
          case "and" => IfC(desugar(l), desugar(r), FalseC())
          case "or" => IfC(desugar(l), TrueC(), desugar(r))
          case "num=" => EqNumC(desugar(l), desugar(r))
          case "num<" => LtC(desugar(l), desugar(r))
          case "num>" => LtC(desugar(r), desugar(l))
          case "cons" => ConsC(desugar(l), desugar(r))
        }
      }

      case UnOpExt(s, e) => {
        s match {
          case "-" => MultC(desugar(NumExt(-1)), desugar(e))
          case "not" => IfC(desugar(e), FalseC(), TrueC())
          case "head" => HeadC(desugar(e))
          case "tail" => TailC(desugar(e))
          case "is-nil" => IsNilC(desugar(e))
          case "is-list" => IsListC(desugar(e))
        }
      }

      case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))

      case NilExt() => NilC()

      case ListExt(l) => {
        l match {
          case x :: tail => ConsC(desugar(x), desugar(ListExt(tail)))
          case Nil => NilC()
          case _ => throw new IException("Invalid input")
        }
      }

      case CondExt(cs) => {
        cs match {
          case (x, y) :: tail => if (desugar(x) == TrueC()) desugar(y) else desugar(CondExt(tail))
          case Nil => throw new IException("Missing else")
          case _ => throw new IException("Invalid input")
        }
      }

      case CondEExt(cs, e) => {
        (cs, e) match {
          case ((x, y) :: tail, e) => IfC(desugar(x), desugar(y), desugar(CondEExt(tail, e)))
          case (Nil, e) => desugar(e)
          case _ => throw new IException("Invalid")
        }
      }

      //
    }
  }
}

object Interp {
  def interp(e: ExprC): Value = {
    e match {
      case NumC(num) => NumV(num)

      case TrueC() => BoolV(true)
      case FalseC() => BoolV(false)

      case PlusC(l, r) => {
        (interp(l), interp(r)) match {
          case (NumV(n), NumV(m)) => NumV(n + m)
          case _ => throw new IException("Invalid input")
        }
      }

      case MultC(l, r) => {
        (interp(l), interp(r)) match {
          case (NumV(n), NumV(m)) => NumV(n * m)
          case _ => throw new IException("Invalid input")
        }
      }

      case EqNumC(l, r) => {
        (interp(l), interp(r)) match {
          case (NumV(n), NumV(m)) =>
            if (n == m) BoolV(true)
            else BoolV(false)
          case _ => throw new IException("Invalid input")
        }
      }

      case LtC(l, r) => {
        (interp(l), interp(r)) match {
          case (NumV(n), NumV(m)) =>
            if (n < m) BoolV(true)
            else BoolV(false)
          case _ => throw new IException("Invalid input")
        }
      }

      //Needs to be done
      case IfC(c, t, d) => {
        interp(c) match {
          case BoolV(true) => interp(t)
          case BoolV(false) => interp(d)
          case _ => throw new IException("Invalid input")
        }
      }

      case NilC() => NilV()
      case ConsC(l, r) => ConsV(interp(l), interp(r))

      case HeadC(e) => {
        interp(e) match {
          case ConsV(head, tail) => head
          case _ => throw new IException("Invalid input")
        }
      }

      case TailC(e) => {
        interp(e) match {
          case ConsV(head, tail) => tail
          case _ => throw new IException("Invalid input")
        }
      }

      case IsListC(e) => {
        interp(e) match {
          case ConsV(head, tail) => BoolV(true)
          case NilV() => BoolV(true)
          case _ => BoolV(false)
        }
      }

      case IsNilC(e) => {
        interp(e) match {
          case NilV() => BoolV(true)
          //case NumV(num) => throw new Interp_Basic.Func_Subst.Func_Env.Mutation.IException("Invalid input")
          case ConsV(head, tail) => BoolV(false)
          case _ => throw new IException("Invalid input")
        }
      }

      case UndefinedC() => throw new IException("Invalid input")
    }
  }
}

