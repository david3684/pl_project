package cs320

import scala.runtime.RichBoolean
import javax.sound.midi.Sequence

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)

  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._
    def isWellFormed(t: Type, tenv: Map[String, Type]): Boolean = t match{
      case IntT => true
      case BooleanT => true
      case UnitT => true
      case AppT(name, targs) => 
        val allArgsWellFormed = targs.forall(arg => isWellFormed(arg, tenv))
        // Step 3
        val tExistsInEnv = tenv.contains(name)
        // Step 4 & 5
        tExistsInEnv && allArgsWellFormed && tenv.get(name).exists {
          case AppT(_, typeParams) => typeParams.length == targs.length
          case _ => false
        }

      case VarT(a) => 
        if(tenv.contains(a)) true
        else false

      case ArrowT(ptypes, rtype) => 
        val allParamsWellFormed = ptypes.forall(arg => isWellFormed(arg, tenv))
        val returnTypeWellFormed = isWellFormed(rtype, tenv)
        if(allParamsWellFormed&&returnTypeWellFormed) true
        else false
      
    }
    def typeCheck(expr: Expr): Type = helperTypeCheck(expr, Map())
    def helperTypeCheck(expr: Expr, tenv: Map[String, Type]): Type = expr match{
      case IntE(_) => IntT
      case BooleanE(_) => BooleanT
      case UnitE => UnitT
      case Add(l, r) => 
        val lt = helperTypeCheck(l, tenv)
        val rt = helperTypeCheck(r, tenv)
        (lt, rt) match {
          case (IntT, IntT) => IntT
          case _ => error("Type Match Failure")
        }
      case Mul(l, r) => 
        val lt = helperTypeCheck(l, tenv)
        val rt = helperTypeCheck(r, tenv)
        (lt, rt) match {
          case (IntT, IntT) => IntT
          case _ => error("Type Match Failure")
        } 
      case Div(l, r) => 
        val lt = helperTypeCheck(l, tenv)
        val rt = helperTypeCheck(r, tenv)
        (lt, rt) match {
          case (IntT, IntT) => IntT
          case _ => error("Type Match Failure")
        }
      case Mod(l, r) => 
        val lt = helperTypeCheck(l, tenv)
        val rt = helperTypeCheck(r, tenv)
        (lt, rt) match {
          case (IntT, IntT) => IntT
          case _ => error("Type Match Failure")
        }
      case Eq(l, r) => 
        val lt = helperTypeCheck(l, tenv)
        val rt = helperTypeCheck(r, tenv)
        (lt, rt) match {
          case (IntT, IntT) => BooleanT
          case _ => error("Type Match Failure")
        }
      case Lt(l, r) => 
        val lt = helperTypeCheck(l, tenv)
        val rt = helperTypeCheck(r, tenv)
        (lt, rt) match {
          case (IntT, IntT) => BooleanT
          case _ => error("Type Match Failure")
        }
      case Typed.Sequence(left, right) => 
        helperTypeCheck(left,tenv)
        val rt = helperTypeCheck(right, tenv)
        rt
      case If(cond, trueBranch, falseBranch) =>
        helperTypeCheck(cond, tenv) match{
          case BooleanT => 
            val tt = helperTypeCheck(trueBranch, tenv) 
            val ft = helperTypeCheck(falseBranch, tenv)
            if(tt==ft) tt
            else error("Type Match Failure")
          case _ => error("Type Match Failure")
        }
    }
  }

  object U {
    import Untyped._

    def interp(expr: Expr): Value = interpHelper(expr, Map(), Map())
    def interpHelper(e: Expr, env: Map[String, Value], sto: Map[String, Value]):Value = e match{
      case IntE(n) => IntV(n)
      case BooleanE(b) => BooleanV(b)
      case Add(l,r) => 
        val lv = interpHelper(l, env, sto)
        val rv = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => IntV(l+r)
          case _ => error("Not Integer")
        }
      case Mul(l,r) => 
        val lv = interpHelper(l, env, sto)
        val rv = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => IntV(l*r)
          case _ => error("Not Integer")
        }
      case Div(l,r) => 
        val lv = interpHelper(l, env, sto)
        val rv = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => if(r!=0) IntV(l/r) else error("Division by Zero")
          case _ => error("Not Integer")
        }
      case Mod(l,r) => 
        val lv = interpHelper(l, env, sto)
        val rv = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => if(r!=0) IntV(l%r) else error("Division by Zero")
          case _ => error("Not Integer")
        }
      case Eq(l,r) => 
        val lv = interpHelper(l, env, sto)
        val rv = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => if(l==r) BooleanV(true) else BooleanV(false)
          case _ => error("Not Integer")
        }
      case Untyped.Sequence(l, r) =>
        interpHelper(l, env, sto)
        val rv = interpHelper(r, env, sto)
        rv
    }
  }
}
