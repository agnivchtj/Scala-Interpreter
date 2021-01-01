package Func_Subst

case class PException(s: String) extends ParseException()
case class DException(s: String) extends DesugarException()
case class IException(s: String) extends InterpException()

object Parser {
  def parse(str: String): ExprExt = parse(Reader.read(str))

  val binOps = List("+", "*", "-", "and", "or", "num=", "num<", "num>", "cons")
  val unOps = List("-", "not", "head", "tail", "is-nil", "is-list")
  val reserved = binOps ::: unOps ::: List("list", "nil", "if", "lambda", "let", "true", "false")

  def parse(sexpr: SExpr): ExprExt = {
    sexpr match {
      case SNum(num) => NumExt(num)

      case SSym("nil") => NilExt()
      case SSym("true") => TrueExt()
      case SSym("false") => FalseExt()

      case SSym(x) => if (!reserved.contains(x)) IdExt(x) else throw new PException("Invalid")

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


      case SList(SSym("cond") :: branches) =>
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


      //List --> (list [Expr*])
      case SList(SSym("list") :: tail) => ListExt(tail.map(x => parse(x)))


      //Lambda --> (lambda ([ID*]) [Expr])
      //case SList(SSym("lambda") :: SList(x :: tail) :: body)
      //FdExt(params: List[String], body: ExprExt)
      //(lambda (x x) (+ x 1))
      case SList(SSym("lambda") :: id_expr_pairs) => {
        id_expr_pairs match {
          case SList(ids) :: body :: Nil =>
            if (
              ids != Nil && checkIds(ids) == true && getIds(ids).distinct.length == getIds(ids).length
            ) FdExt(getIds(ids), parse(body))
            else if (ids == Nil) FdExt(Nil, parse(body))
            else throw new PException("Invalid")

          case _ => throw new PException("Invalid")
        }
      }


      //AppExt(f: ExprExt, args: List[ExprExt])
      //((lambda (x y) (* x y)) 3 2)
      //((lambda (x) ((lambda (y) (+ x y)) 2)) 3)
      case SList(SList(x) :: args) => AppExt(
        parse(SList(x)),
        args.map(i => parse(i))
      )


      //Let --> (let ((<id1> <expr1>) (<id2> <expr2>)) <body>)
      //        (let ([LetBind+]) [Expr])
      //LetExt(binds: List[LetBindExt], body: ExprExt)
      //(let ((x 1)) (+ y 1))
      //(let ((f (lambda (x) (* x 1)))) (f 3))
      case SList(SSym("let") :: id_exprs_body) => {
        id_exprs_body match {
          case SList(let_binds) :: body :: Nil =>
            if (
              checkBinds(let_binds) == true
                && getBinds(let_binds).size >= 1
                && getBinds(let_binds).map(_.name).distinct.length == getBinds(let_binds).map(_.name).length
            )
              LetExt(getBinds(let_binds), parse(body))
            else throw new PException("Invalid")

          case _ => throw new PException("Invalid")
        }
      }

      //AppExt(IdExt("f"), List(NumExt(3)))
      case SList(SSym(x) :: tail) => AppExt(
        IdExt(x),
        tail.map(i => parse(i))
      )


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

  def checkIds(ids: List[SExpr]): Boolean = {
    ids match {
      case SSym(x) :: tail => if (!reserved.contains(x)) checkIds(tail) else false
      case Nil => true
      case _ => throw new PException("Invalid")
    }
  }

  def getIds(ids: List[SExpr]): List[String] = {
    ids match {
      case SSym(x) :: tail => x :: getIds(tail)
      case Nil => Nil
      case _ => throw new PException("Invalid")
    }
  }

  def checkBinds(binds: List[SExpr]): Boolean = {
    binds match {
      case SList(SSym(x) :: y :: Nil) :: tail => if (!reserved.contains(x)) checkBinds(tail) else false
      case Nil => true
      case _ => throw new PException("Invalid")
    }
  }

  def getBinds(binds: List[SExpr]): List[LetBindExt] = {
    binds match {
      case SList(SSym(x) :: y :: Nil) :: tail => LetBindExt(x, parse(y)) :: getBinds(tail)
      case Nil => Nil
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

      //
    }
  }
}

object Interp {
  //subst(expr: ExprC, bnds: List[Bind]): ExprC
  //Bind = (String, Value)
  def subst(e: ExprC, bnds: List[Bind]): ExprC = {
    e match {
      case TrueC() => TrueC()
      case FalseC() => FalseC()

      case NumC(num) => NumC(num)
      case PlusC(l, r) => PlusC(subst(l, bnds), subst(r, bnds))
      case MultC(l, r) => MultC(subst(l, bnds), subst(r, bnds))

      case IfC(c, t, e) => IfC(subst(c, bnds), subst(t, bnds), subst(e, bnds))
      case EqNumC(l, r) => EqNumC(subst(l, bnds), subst(r, bnds))
      case LtC(l, r) => LtC(subst(l, bnds), subst(r, bnds))

      case NilC() => NilC()
      case ConsC(l, r) => ConsC(subst(l, bnds), subst(r, bnds))
      case HeadC(e) => HeadC(subst(e, bnds))
      case TailC(e) => TailC(subst(e, bnds))

      case IsNilC(e) => IsNilC(subst(e, bnds))
      case IsListC(e) => IsListC(subst(e, bnds))

      //Needs to be done
      //AppC(FdC(List("y"), PlusC(IdC("x"), IdC("y"))), List(NumC(2))),
      //List(  Bind("x", NumC(3))  )
      //AppC( subst(FdC(List("y"), PlusC(IdC("x"), IdC("y"))), List(  Bind("x", NumC(3))  )), List(NumC(2)))
      case AppC(f, args) => AppC(subst(f, bnds), args.map(i => subst(i, bnds)))

      case IdC(c) =>
        if (bnds.filter(_.name == c).length < 1) IdC(c)
        else if (bnds.filter(_.name == c).length >= 1) ValC(bnds.filter(i => i.name == c).head.value)
        else throw new IException("Invalid")

      //We only want the strings which are not in parameters
      case FdC(params, body) => FdC(
        params,
        subst(body, bnds.filter(i => (bnds.map(_.name) diff params).contains(i.name)))
      )

      case ValC(v) => ValC(v)

      case UndefinedC() => throw new IException("Invalid input")
    }
  }

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
          //case NumV(num) => throw new Func_Subst.IException("Invalid input")
          case ConsV(head, tail) => BoolV(false)
          case _ => throw new IException("Invalid input")
        }
      }

      case UndefinedC() => throw new IException("Invalid input")

      //AppC(f: ExprC, args: List[ExprC])
      //interp(subst(body, List(  Bind("x", NumC(3))  )))
      case AppC(f, args) => {
        interp(f) match {
          case FunV(func) => {
            func match {
              case FdC(params, body) =>
                if (params.size == args.size) interp(
                  subst(body, params.zip(args).map(i => Bind(i._1, interp(i._2))))
                )
                else throw new IException("Invalid")

            }
          }

          case _ => throw new IException("Invalid function")
        }
      }

      case IdC(c) => throw new IException("Invalid")

      case FdC(params, body) => FunV(FdC(params, body))

      case ValC(v) => v

      case _ => throw new IException("Invalid")
    }
  }
}

