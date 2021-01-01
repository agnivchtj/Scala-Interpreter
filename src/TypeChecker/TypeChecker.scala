package TypeChecker

import Library._
import Typed._
import Parser._

case class DException(s: String) extends DesugarException(s)
case class IException(s: String) extends InterpException(s)
case class TException(s: String) extends TypeException(s)

object Desugar {
  def desugar(e: ExprExt): ExprC = {
    e match {
      case NumExt(n) => NumC(n)

      case TrueExt() => TrueC()
      case FalseExt() => FalseC()

      case NilExt(listTy) => NilC()
      case IfExt(c, t, e) => IfC(desugar(c), desugar(t), desugar(e))

      case ListExt(listTy, es) => {
        es match {
          case e :: tail => ConsC(desugar(e), desugar(ListExt(listTy, tail)))
          case Nil => NilC()
          case _ => throw new IException("Invalid input")
        }
      }

      case BinOpExt(s, l, r) => {
        s match {
          case "+" => PlusC(desugar(l), desugar(r))
          case "*" => MultC(desugar(l), desugar(r))
          case "-" => PlusC(desugar(l), MultC(NumC(-1), desugar(r)))

          case "and" => IfC(desugar(l), desugar(r), FalseC())
          case "or" => IfC(desugar(l), TrueC(), desugar(r))
          case "num=" => EqNumC(desugar(l), desugar(r))
          case "num<" => LtC(desugar(l), desugar(r))
          case "num>" => LtC(desugar(r), desugar(r))
          case "cons" => ConsC(desugar(l), desugar(r))

          case "setbox" => SetboxC(desugar(l), desugar(r))
          case "seq" => SeqC(desugar(l), desugar(r))

          case "pair" => PairC(desugar(l), desugar(r))

          case _ => throw new DException("Invalid")
        }
      }

      case UnOpExt(s, e) => {
        s match {
          case "-" => MultC(desugar(NumExt(-1)), desugar(e))

          case "not" => IfC(desugar(e), FalseC(), TrueC())
          case "head" => HeadC(desugar(e))

          case "tail" => TailC(desugar(e))
          case "is-nil" => IsNilC(desugar(e))

          case "box" => BoxC(desugar(e))
          case "unbox" => UnboxC(desugar(e))

          case "fst" => FstC(desugar(e))
          case "snd" => SndC(desugar(e))

          case _ => throw new DException("Invalid")
        }
      }

      case IdExt(id) => IdC(id)
      case SetExt(id, e) => SetC(id, desugar(e))

      case AppExt(f, args) => AppC(desugar(f), args.map(i => desugar(i)))

      case FdExt(params, body) => FdC(params.map(i => i.name), desugar(body))

      case LetExt(binds, body) => AppC(
        FdC(binds.map(i => i.name), desugar(body)),
        binds.map(i => desugar(i.value))
      )

      //name: String, paramTy: Type, retTy: Type, param: String, body: ExprExt
      case RecLamExt(name, paramTy, retTy, param, body) => AppC(
        desugar(parse("(lambda (f) ((lambda (x) (f (lambda (a) ((x x) a)))) (lambda (x) (f (lambda (a) ((x x) a))))))")),
        List(FdC(List(name), FdC(List(param), desugar(body))))
      )

      //binds: List[LetRecBindExt], body: ExprExt
      //LetRecBindExt(name: String, ty: Type, value: ExprExt)
      case LetRecExt(binds, body) => AppC(
        FdC(binds.map(i => i.name), process(binds, body)),
        binds.map(i => UninitializedC())
      )

      case _ => throw new DException("Invalid")
    }
  }

  def process(bnds: List[LetRecBindExt], body: ExprExt): ExprC = {
    bnds match {
      case LetRecBindExt(name, ty, value) :: tail => SeqC(
        SetC(bnds.head.name, desugar(bnds.head.value)),
        process(tail, body)
      )
      case Nil => desugar(body)
      case _ => throw new DException("Invalid")
    }
  }
}

object Interp {
  type Store = List[Cell]
  type PointerEnvironment = List[Pointer]

  // Do not remove this method. We use this for grading.
  def interp(e: ExprC): Value = interp(e, Nil, Nil)._1

  def interp(e: ExprC, nv: PointerEnvironment, st1: Store): (Value, Store) = {
    e match {
      case TrueC() => (BoolV(true), st1)
      case FalseC() => (BoolV(false), st1)

      case NumC(n) => (NumV(n), st1)

      case PlusC(l, r) => {
        val (lv, st2) = interp(l, nv, st1)
        val (rv, st3) = interp(r, nv, st2)
        (lv, rv) match {
          case (NumV(n), NumV(m)) => (NumV(n + m), st3)
          case _ => throw new IException("Invalid")
        }
      }

      case MultC(l, r) => {
        val (lv, st2) = interp(l, nv, st1)
        val (rv, st3) = interp(r, nv, st2)
        (lv, rv) match {
          case (NumV(n), NumV(m)) => (NumV(n * m), st3)
          case _ => throw new IException("Invalid")
        }
      }

      case EqNumC(l, r) => {
        val (lv, st2) = interp(l, nv, st1)
        val (rv, st3) = interp(r, nv, st2)
        (lv, rv) match {
          case (NumV(n), NumV(m)) =>
            if (n == m) (BoolV(true), st3)
            else (BoolV(false), st3)
          case _ => throw new IException("Invalid")
        }
      }

      case LtC(l, r) => {
        val (lv, st2) = interp(l, nv, st1)
        val (rv, st3) = interp(r, nv, st2)
        (lv, rv) match {
          case (NumV(n), NumV(m)) =>
            if (n < m) (BoolV(true), st3)
            else (BoolV(false), st3)
          case _ => throw new IException("Invalid")
        }
      }

      case IfC(c, t, e) => {
        val (cv, st2) = interp(c, nv, st1)
        cv match {
          case BoolV(true) => interp(t, nv, st2)
          case BoolV(false) => interp(e, nv, st2)
          case _ => throw new IException("Invalid input")
        }
      }

      case NilC() => (NilV(), st1)
      case ConsC(l, r) => {
        val (lv, st2) = interp(l, nv, st1)
        val (rv, st3) = interp(r, nv, st2)
        (ConsV(lv, rv), st3)
      }

      case HeadC(e) => {
        val (ev, st2) = interp(e, nv, st1)
        ev match {
          case ConsV(head, tail) => (head, st2)
          case _ => throw new IException("Invalid")
        }
      }

      case TailC(e) => {
        val (ev, st2) = interp(e, nv, st1)
        ev match {
          case ConsV(head, tail) => (tail, st2)
          case _ => throw new IException("Invalid input")
        }
      }

      case IsNilC(e) => {
        val (ev, st2) = interp(e, nv, st1)
        ev match {
          case NilV() => (BoolV(true), st2)
          case ConsV(head, tail) => (BoolV(false), st2)
          case _ => throw new IException("Invalid input")
        }
      }

      case AppC(f, args) => {
        val (fv, st2) = interp(f, nv, st1)

        fv match {
          case PointerClosV(func, env) => {
            func match {
              case FdC(params, body) =>
                val (argsv, st3) = interpArgs(args, nv, st2)

                if (params.size == args.size)
                  interp(body, updateEnv(params, argsv, st3, Nil)._1 ::: env, updateEnv(params, argsv, st3, Nil)._2)
                else throw new IException("Invalid")
            }
          }

          case _ => throw new IException("Invalid function")
        }
      }

      case IdC(s) => (fetch(lookup(s, nv), st1), st1)

      case FdC(params, body) => (PointerClosV(FdC(params, body), nv), st1)

      case SetC(v, b) => {
        interp(b, nv, st1) match {
          case (value, st2) => (value, updateStore(st2, lookup(v, nv), value))
          case _ => throw new IException("Invalid")
        }
      }

      case SeqC(b1, b2) => {
        val (b1v, st2) = interp(b1, nv, st1)
        val (b2v, st3) = interp(b2, nv, st2)
        (b2v, st3)
      }

      case BoxC(v) => {
        val (vv, st2) = interp(v, nv, st1)
        (BoxV(newLoc(st2)), updateStore(st2, newLoc(st2), vv))
      }

      case UnboxC(b) => {
        val (bv, st2) = interp(b, nv, st1)
        (bv, st2) match {
          case (BoxV(l), _) => (fetch(l, st2), st2)
          case _ => throw new IException("Invalid")
        }
      }

      case SetboxC(b, v) => {
        val (bv, st2) = interp(b, nv, st1)
        val (vv, st3) = interp(v, nv, st2)
        (bv, st2) match {
          case (BoxV(l), _) => (vv, updateStore(st3, l, vv))
          case _ => throw new IException("Invalid")
        }
      }

      case UninitializedC() => (UninitializedV(), st1)

      case PairC(l, r) => {
        val (lv, st2) = interp(l, nv, st1)
        val (rv, st3) = interp(r, nv, st2)
        (PairV(lv, rv), st3)
      }

      case FstC(e) => {
        interp(e, nv, st1) match {
          case (PairV(fst, snd), st2) => (fst, st2)
          case _ => throw new IException("Invalid")
        }
      }

      case SndC(e) => {
        interp(e, nv, st1) match {
          case (PairV(fst, snd), st2) => (snd, st2)
          case _ => throw new IException("Invalid")
        }
      }

      case _ => throw new IException("Invalid")
    }
  }


  //Helper functions
  def lookup(s: String, nv: PointerEnvironment): Int = {
    nv match {
      case Pointer(name, location) :: tail => if (s == name) location else lookup(s, tail)
      case _ => throw new IException("Invalid")
    }
  }

  def fetch(loc: Int, st: Store): Value = {
    st match {
      case Cell(location, value) :: tail => if (loc == location) value else fetch(loc, tail)
      case _ => throw new IException("Invalid")
    }
  }

  def newLoc(st: Store): Int = st.size + 1

  def updateStore(st: Store, location: Int, value: Value): Store = {
    Cell(location, value) :: update(st, location)
  }

  def update(st: Store, location: Int): Store = {
    st match {
      case Cell(loc, v) :: tail =>
        if (loc != location) Cell(loc, v) :: update(tail, location)
        else update(tail, location)
      case Nil => Nil
      case _ => throw new IException("Invalid")
    }
  }

  def interpArgs(args: List[ExprC], nv: PointerEnvironment, st: Store): (List[Value], Store) = {
    args match {
      case arg :: tail => {
        val (argv, st2) = interp(arg, nv, st)
        val (tailv, st3) = interpArgs(tail, nv, st2)
        (argv :: tailv, st3)
      }
      case Nil => (Nil, st)
      case _ => throw new IException("Invalid")
    }
  }

  def updateEnv(params: List[String], argsv: List[Value], st: Store, nv: PointerEnvironment): (PointerEnvironment, Store) = {
    (params, argsv) match {
      case (param :: tail, argv :: argsv) =>
        val location = newLoc(st)
        val pter = Pointer(param, location)
        updateEnv(tail, argsv, updateStore(st, location, argv), pter :: nv)
      case (Nil, Nil) => (nv, st)
      case _ => throw new IException("Does not match list")
    }
  }
}


object TypeChecker {
  type TEnvironment = List[TBind]

  def typeOf(e: ExprExt): Type = typeOf(e, Nil)

  def typeOf(e: ExprExt, nv: TEnvironment): Type = {
    e match {
      case TrueExt() => BoolT()
      case FalseExt() => BoolT()
      case NumExt(num) => NumT()

      case NilExt(listTy) => ListT(listTy)

      case BinOpExt(s, l, r) => {
        s match {
          case "+" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => NumT()
              case _ => throw new TException("Invalid")
            }
          }

          case "*" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => NumT()
              case _ => throw new TException("Invalid")
            }
          }

          case "-" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => NumT()
              case _ => throw new TException("Invalid")
            }
          }

          case "and" => typeOf(IfExt(l, r, FalseExt()))

          case "or" => typeOf(IfExt(l, TrueExt(), r))

          case "num=" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => BoolT()
              case _ => throw new TException("Invalid")
            }
          }

          case "num<" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => BoolT()
              case _ => throw new TException("Invalid")
            }
          }

          case "num>" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (NumT(), NumT()) => BoolT()
              case _ => throw new TException("Invalid")
            }
          }


          case "cons" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (typ1, typ2) => if (ListT(typ1) == typ2) typ2 else throw new TException("Mismatch")
              case _ => throw new TException("Invalid")
            }
          }

          //pair 1 true => (Pair Num Bool)
          //pair (pair 1 true) (nil : Num) => (Pair (Pair Num Bool) (List Num))
          case "pair" => PairT(typeOf(l, nv), typeOf(r, nv))

          //BinOpExt("setbox", UnOpExt("box", NumExt(1)), NumExt(2))
          //(setbox (box 1) 2)
          case "setbox" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (RefT(t), typ) => if (t == typ) t else throw new TException("Invalid")

              case _ => throw new TException("Invalid")
            }
          }

          //(let ((x 0)) (seq (set x 42) x))
          //(let ((x (box 0))) (seq (setbox x 42) (unbox x)))
          case "seq" => {
            (typeOf(l, nv), typeOf(r, nv)) match {
              case (typ1, typ2) => if (typ1 == typ2) typ2 else throw new TException("Invalid")
              case _ => throw new TException("Invalid")
            }
          }

          case _ => throw new TException("Invalid")
        }
      }

      case UnOpExt(s, e) => {
        s match {
          case "-" => {
            typeOf(e, nv) match {
              case NumT() => NumT()
              case _ => throw new TException("Invalid")
            }
          }

          case "not" => typeOf(IfExt(e, FalseExt(), TrueExt()))

          case "head" => {
            typeOf(e, nv) match {
              case ListT(expTy) => expTy
              case _ => throw new TException("Invalid")
            }
          }

          case "tail" => {
            typeOf(e, nv) match {
              case ListT(expTy) => ListT(expTy)
              case _ => throw new TException("Invalid")
            }
          }

          case "is-nil" => {
            typeOf(e, nv) match {
              case ListT(expTy) => BoolT()
              case _ => throw new TException("Invalid")
            }
          }

          //fst (pair 1 true) => Num
          case "fst" => {
            typeOf(e, nv) match {
              case PairT(fst, snd) => fst
              case _ => throw new TException("Invalid")
            }
          }

          //snd (pair 1 true) => Bool
          case "snd" => {
            typeOf(e, nv) match {
              case PairT(fst, snd) => snd
              case _ => throw new TException("Invalid")
            }
          }

          //(let ((b (box 1))) (seq (setbox b 10) (unbox b)))
          case "box" => RefT(typeOf(e, nv))

          //(unbox (box 1))
          case "unbox" => {
            typeOf(e, nv) match {
              case RefT(t) => t

              case _ => throw new TException("Invalid")
            }
          }

          //
        }
      }

      case IfExt(c, t, e) => {
        typeOf(c, nv) match {
          case BoolT() => {
            (typeOf(t, nv), typeOf(e, nv)) match {
              case (NumT(), NumT()) => NumT()
              case (BoolT(), BoolT()) => BoolT()
              case _ => throw new TException("Invalid")
            }
          }

          case _ => throw new TException("Invalid")
        }
      }

      case SetExt(id, e) => {
        (lookup(id, nv), typeOf(e, nv)) match {
          case (typ_id, typ_ext) => if (typ_id == typ_ext) typ_ext else throw new TException("Invalid")
          case _ => throw new TException("Invalid")
        }
      }

      case IdExt(c) => lookup(c, nv)

      //FdExt(params: List[Param], body: ExprExt)
      //Param(name: String, ty: Type)
      case FdExt(params, body) => FunT(
        params.map(i => i.ty),
        typeOf(
          body,
          params.map(i => TBind(i.name, i.ty)) ::: nv
        )
      )

      //AppExt(f: ExprExt, args: List[ExprExt])
      //
      case AppExt(f, args) => {
        typeOf(f, nv) match {
          case FunT(paramTy, retTy) => {
            val args_typ = args.map(i => typeOf(i, nv))
            if (paramTy.equals(args_typ)) retTy else throw new TException("Type mismatch")
          }

          case _ => throw new TException("Invalid")
        }
      }


      //LetExt(binds: List[LetBindExt], body: ExprExt)
      //LetBindExt(name: String, value: ExprExt)
      //(let ((b (box 1))) (seq (setbox b 10) (unbox b)))
      case LetExt(binds, body) =>
        val env = nv ::: binds.map(i => TBind(i.name, typeOf(i.value, nv)))
        typeOf(body, env)

      //ListExt(listTy: Type, es: List[ExprExt])
      //(list 1 2 3) => (List Num)
      case ListExt(listTy, es) => {
        if (checkList(es, listTy, nv) == true) ListT(listTy)
        else throw new TException("Invalid")
      }

      //LetRecExt(binds: List[LetRecBindExt], body: ExprExt)
      //LetRecBindExt(name: String, ty: Type, value: ExprExt)
      //(letrec (((x : Num) x)) x)
      //LetRecExt(LetRecBindExt("x", NumT(), IdExt("x")) :: Nil, IdExt("x"))
      case LetRecExt(binds, body) =>
        val env = nv ::: binds.map(i => TBind(i.name, i.ty))
        if (checkType(binds, env) == true) typeOf(body, env)
        else throw new TException("Invalid")

      //(rec-lam (f : Num -> Num) (x) (if (num= x 0) 0 (+ x 1)))
      //FunT(NumT() :: Nil, NumT())
      //FunT(paramTy: List[Type], retTy: Type)
      case RecLamExt(name, paramTy, retTy, param, body) =>
        val env = List(TBind(param, paramTy), TBind(name, FunT(List(paramTy), retTy))) ::: nv
        if (typeOf(body, env) == retTy) typeOf(IdExt(name), env)
        else throw new TException("Invalid")

      case _ => throw new TException("Invalid")
    }
  }

  def lookup(x: String, nv: TEnvironment): Type = {
    nv match {
      case TBind(name, ty) :: tail => if (name == x) ty else lookup(x, tail)
      case _ => throw new TException("Invalid")
    }
  }

  def checkList(es: List[ExprExt], listTy: Type, nv: TEnvironment): Boolean = {
    es match {
      case e :: tail => if (typeOf(e, nv) == listTy) checkList(tail, listTy, nv) else false
      case Nil => true
      case _ => throw new TException("Invalid")
    }
  }

  def checkType(bnds: List[LetRecBindExt], nv: TEnvironment): Boolean = {
    bnds match {
      case LetRecBindExt(name, ty, value) :: tail =>
        if (typeOf(value, nv) == ty) checkType(tail, nv)
        else false
      case Nil => true
      case _ => throw new TException("Invalid")
    }
  }
}


object SafeInterp {
  def interp(e: ExprExt): Value = {
    TypeChecker.typeOf(e)
    Interp.interp(Desugar.desugar(e))
  }
}

