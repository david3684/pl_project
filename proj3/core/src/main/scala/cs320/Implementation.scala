package cs320

import scala.runtime.RichBoolean
import javax.sound.midi.Sequence

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)

  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._

    case class TypeScheme(typeVars: List[String], t: Type)

    type Mutability = Boolean

    type TypeEnv = Map[String, Either[(TypeScheme, Mutability), TypeDef]]
    def isWellFormed(t: Type, tenv: TypeEnv): Boolean = t match{
      case IntT => true
      case BooleanT => true
      case UnitT => true
      case AppT(name, targs) => 
        val allArgsWellFormed = targs.forall(arg => isWellFormed(arg, tenv))
        val tExistsInEnv = tenv.contains(name)
        tExistsInEnv && allArgsWellFormed && tenv.get(name).exists {
          case Right(TypeDef(_,tparams, _)) => tparams.length == targs.length
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
    def substituteType(t: Type, typeVars: List[String], typeArgs: List[Type]): Type = {
      def substitute(t: Type): Type = t match {
        case VarT(name) => 
          typeVars.zip(typeArgs).toMap.getOrElse(name, t)
        case AppT(name, targs) => 
          AppT(name, targs.map(substitute))
        case ArrowT(ptypes, rtype) => 
          ArrowT(ptypes.map(substitute), substitute(rtype))
        case _ => t
      }
      substitute(t)
    }
    def typeCheck(expr: Expr): Type = helperTypeCheck(expr, Map.empty[String, Either[(TypeScheme, Mutability), TypeDef]])
    def helperTypeCheck(expr: Expr, tenv: TypeEnv): Type = expr match{
      case IntE(_) => IntT
      case BooleanE(_) => BooleanT
      case UnitE => UnitT
      case Id(x, types) => 
        types match{
          case List() => {
            val Left((xt, mut)) = tenv.getOrElse(x, error(s"No type for identifier: $x"))
            xt match {
              case TypeScheme(tvars, t) => {
                t
              }
              case _ => error("Error")
            }
          }
          case _ =>{
            val allTypesWellFormed = types.forall(arg => isWellFormed(arg, tenv))
            if(allTypesWellFormed) {
              val Left((xt, mut)) = tenv.getOrElse(x, error(s"No type for identifier: $x"))
              xt match {
                case TypeScheme(tvars, t) => {
                  if(tvars.length == 0) t
                  else if (tvars.length == types.length) {
                    substituteType(t, tvars, types)
                  }
                  else error("error")
                }
                case _ => error("error")
              }
            }
            else {
              error("Some Types are not Well-Formed")
            }
            
          }
        }
        
        

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
      case Val(mut, name , typ, e, b) => 
        typ match {
          case Some(t1) =>{
            isWellFormed(t1, tenv) match{
              case true => {
                val et = helperTypeCheck(e, tenv)
                if(t1==et){
                  val newtenv = tenv + (name -> Left((TypeScheme(List(), et), mut)))
                  helperTypeCheck(b, newtenv)
                }
                else error("ERROR")    
              }
              case false => error("Not Well-Formed Type")
            }
          }
          case None => {
            val et = helperTypeCheck(e, tenv)
            val newtenv = tenv + (name -> Left((TypeScheme(List(), et), mut)))
            helperTypeCheck(b, newtenv)
          }
        }
      
      case Fun(params, b) => {
        if(params.forall(arg => isWellFormed(arg._2, tenv))){
          val updatedEnv = params.foldLeft(tenv) { (accEnv, param) =>
            accEnv + (param._1 -> Left((TypeScheme(List(), param._2), true))) // Assuming true for val (non-mutable)
          }
          val bodyType = helperTypeCheck(b, updatedEnv)
          val funcType = ArrowT(params.map(_._2), bodyType)
          funcType
        }
        else error("Some Types are Not Well-Formed")
      }
    }
  }

  object U {
    import Untyped._
    type Sto = Map[Addr, Value]
    type Env = Map[String, Addr]
    def malloc(sto: Sto): Addr = (sto.keySet + 0).max + 1
    /*def strict(v: Value, s: Sto): Value = v match {
      case ExprV(e, env) => strict(interpHelper(e, env, s))
      case _ => v
    }*/
    def interp(expr: Expr): Value = interpHelper(expr, Map(), Map()) match{
      case (v, _) => v
    }
    def interpHelper(e: Expr, env: Env, sto: Sto):(Value, Sto) = e match{ 
      case IntE(n) => (IntV(n), sto)
      case BooleanE(b) => (BooleanV(b), sto)
      case Id(x) => 
        val addr = env.getOrElse(x, error(s"free identifier: $x"))
        val xv = sto.getOrElse(addr, error(s"free identifier: $x"))
        xv match{
          case ExprV(e, xenv) =>{
            val (ev, es) = interpHelper(e, xenv, sto)
            (ev, es+(addr->ev))
          }
          case _ => (xv, sto)
        }
      case Add(l,r) => 
        val ((lv, ls)) = interpHelper(l, env, sto)
        val ((rv, rs)) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => (IntV(l+r), sto)
          case _ => error("Not Integer")
        }
      case Mul(l,r) => 
        val (lv, ls) = interpHelper(l, env, sto)
        val ((rv, rs)) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => (IntV(l*r), sto)
          case _ => error("Not Integer")
        }
      case Div(l,r) => 
        val (lv, ls) = interpHelper(l, env, sto)
        val (rv, rs) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => if(r!=0) (IntV(l/r),sto) else error("Division by Zero")
          case _ => error("Not Integer")
        }
      case Mod(l,r) => 
        val (lv, ls) = interpHelper(l, env, sto)
        val (rv, rs) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => if(r!=0) (IntV(l%r),sto) else error("Division by Zero")
          case _ => error("Not Integer")
        }
      case Eq(l,r) => 
        val (lv, ls) = interpHelper(l, env, sto)
        val (rv, rs) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => if(l==r) (BooleanV(true),sto) else (BooleanV(false), sto)
          case _ => error("Not Integer")
        }
      case Untyped.Sequence(l, r) =>
        interpHelper(l, env, sto)
        val (rv, rs) = interpHelper(r, env, sto)
        (rv,sto)
      case Val(name, e, b) => {
        val (ev, es) = interpHelper(e,env,sto)
        val addr = malloc(es)
        interpHelper(b, env + (name->addr), es + (addr -> ev))
      }

    }
  }
}
