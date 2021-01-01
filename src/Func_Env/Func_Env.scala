package Func_Env

import Library._
import Parser._
import Untyped._

// IMPORTANT: DO NOT USE SUBSTITUTION FOR THIS ASSIGNMENT
case class DException(s: String) extends DesugarException()
case class IException(s: String) extends InterpException()

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
          case (x, y) :: tail => IfC(desugar(x), desugar(y), desugar(CondExt(tail)))
          case Nil => throw new IException("Missing else")
          case _ => throw new IException("Invalid input")
        }
      }

      case CondEExt(cs, e) => {
        (cs, e) match {
          case (_, NilExt()) => throw new IException("Missing else")
          case ((x, y) :: tail, e) => IfC(desugar(x), desugar(y), desugar(CondEExt(tail, e)))
          case (Nil, _) => desugar(e)
        }
      }

      //AppExt(f: ExprExt, args: List[ExprExt])
      case AppExt(f, args) => AppC(desugar(f), args.map(x => desugar(x)))

      //IdExt(c: String)
      case IdExt(c) => IdC(c)

      //FdExt(params: List[String], body: ExprExt)
      case FdExt(params, body) => FdC(params, desugar(body))

      //LetExt(binds: List[LetBindExt], body: ExprExt)
      //LetExt(List(LBE("x", NumExt(1)), LBE("y", NumExt(2))), BinOpExt("+", IDE("x"), IDE("y")))
      //AppC(FdC(List("x"), PlusC(IdC("y"), NumC(1))), List(NumC(1)))
      case LetExt(binds, body) => AppC(
        FdC(binds.map(_.name), desugar(body)),
        binds.map(i => desugar(i.value))
      )

      //RecLamExt(name: String, param: String, body: ExprExt)
      //RecLamExt("f", "x", IfExt(BinOpExt("num=", IdExt("x"), NumExt(0)), NumExt(0), BinOpExt("+", IdExt("x"), NumExt(1))))
      //AppC(<Y> FdC(List("f"), FdC(List("x"), IfC(EqNumC(IdC("x"), NumC(0)), NumC(0), PlusC(IdC("x"), NumC(1))))))
      case RecLamExt(name, param, body) => AppC(
        desugar(parse("(lambda (f) ((lambda (x) (f (lambda (a) ((x x) a)))) (lambda (x) (f (lambda (a) ((x x) a))))))")),
        List(FdC(List(name), FdC(List(param), desugar(body))))
      )

      //
    }
  }
}

object Interp {
  type Env = List[Bind]

  def interp(e: ExprC, nv: Env): Value = {
    e match {
      case NumC(num) => NumV(num)

      case TrueC() => BoolV(true)
      case FalseC() => BoolV(false)

      case PlusC(l, r) => {
        (interp(l, nv), interp(r, nv)) match {
          case (NumV(n), NumV(m)) => NumV(n + m)
          case _ => throw new IException("Invalid input")
        }
      }

      case MultC(l, r) => {
        (interp(l, nv), interp(r, nv)) match {
          case (NumV(n), NumV(m)) => NumV(n * m)
          case _ => throw new IException("Invalid input")
        }
      }

      case EqNumC(l, r) => {
        (interp(l, nv), interp(r, nv)) match {
          case (NumV(n), NumV(m)) =>
            if (n == m) BoolV(true)
            else BoolV(false)
          case _ => throw new IException("Invalid input")
        }
      }

      case LtC(l, r) => {
        (interp(l, nv), interp(r, nv)) match {
          case (NumV(n), NumV(m)) =>
            if (n < m) BoolV(true)
            else BoolV(false)
          case _ => throw new IException("Invalid input")
        }
      }

      //Needs to be done
      case IfC(c, t, d) => {
        interp(c, nv) match {
          case BoolV(true) => interp(t, nv)
          case BoolV(false) => interp(d, nv)
          case _ => throw new IException("Invalid input")
        }
      }

      case NilC() => NilV()
      case ConsC(l, r) => ConsV(interp(l, nv), interp(r, nv))

      case HeadC(e) => {
        interp(e, nv) match {
          case ConsV(head, tail) => head
          case _ => throw new IException("Invalid input")
        }
      }

      case TailC(e) => {
        interp(e, nv) match {
          case ConsV(head, tail) => tail
          case _ => throw new IException("Invalid input")
        }
      }

      case IsListC(e) => {
        interp(e, nv) match {
          case ConsV(head, tail) => BoolV(true)
          case NilV() => BoolV(true)
          case _ => BoolV(false)
        }
      }

      case IsNilC(e) => {
        interp(e, nv) match {
          case NilV() => BoolV(true)
          case ConsV(head, tail) => BoolV(false)
          case _ => throw new IException("Invalid input")
        }
      }

      case UndefinedC() => throw new IException("Invalid input")

      //ClosV(f: FdC, env: Environment)
      //AppC(FdC(List("x"), MultC(IdC("x"), NumC(2))), List(NumC(3)))
      //params zip args = List( ("x", NumC(3)) )
      case AppC(f, args) => {
        interp(f, nv) match {
          case ClosV(func, env) => {
            func match {
              case FdC(params, body) =>
                if (params.size == args.size) interp(
                  body,
                  params.zip(args).map(i => Bind(i._1, interp(i._2, nv))) ::: env
                )
                else throw new IException("Invalid")
            }
          }

          case _ => throw new IException("Invalid function")
        }
      }

      case IdC(c) => {
        nv match {
          case Bind(name, value) :: tail => if (c == name) value else interp(IdC(c), tail)
          case _ => throw new IException("Invalid")
        }
      }

      case FdC(params, body) => ClosV(FdC(params, body), nv)

      case ValC(v) => v

      case _ => throw new IException("Invalid")
    }
  }

  // IMPORTANT: DO NOT USE SUBSTITUTION FOR THIS ASSIGNMENT

  def interp(e: ExprC): Value = interp(e, Nil)
}
