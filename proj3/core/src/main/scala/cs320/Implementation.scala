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
      case Assign(name, expr) => {
        val Left((xt, mut)) = tenv.getOrElse(name, error(s"No type for identifier: $name"))
        xt match{
          case TypeScheme(typevars, t) => if(typevars.length == 0){
            if(!mut){
              val et = helperTypeCheck(expr, tenv)
              if(et==t) UnitT
              else error("ERROR")
            }
            else error("ERROR")
          }
          else error("ERROR")
        }
      }
      case App(fun, args) => 
        helperTypeCheck(fun, tenv) match {
          case ArrowT(ptypes, rtype) => {
            if(args.length != ptypes.length) error("ERROR")
            else{
              args.zip(ptypes).foreach { case (arg, ptype) =>
                val argType = helperTypeCheck(arg, tenv)
                if (argType != ptype) error(s"Type mismatch: expected $ptype, found $argType")
              }
              rtype
            }
          }
          case _ => error("Function type expected")
        }
      case Match(expr, cases) => {
        val et = helperTypeCheck(expr, tenv)
        et match {
          case AppT(typeName, typeArgs) =>
            tenv.get(typeName) match {
              case Some(Right(TypeDef(_, typeParams, _))) =>
                val substitutionMap = typeParams.zip(typeArgs).toMap
                if (cases.length != typeParams.length) error("Number of cases and type parameters mismatch")
                val caseTypes = cases.map { caseObj =>
                  val caseType = helperTypeCheck(caseObj.body, tenv)
                  substituteType(caseType, typeParams, typeArgs)
                }
                if (caseTypes.forall(_ == caseTypes.head)) caseTypes.head
                else error("All cases must have the same type")
              case _ => error("Type application expected")
            }
          case _ => error("Type application expected in match expression")
        }
      }
      case RecBinds(defs, body) => {
        def processDef(accEnv: TypeEnv, d: TypeDef): TypeEnv = {
          d match {
            case TypeDef(typeName, typeParams, w) =>
              // Check if the type name already exists in the environment
              if (accEnv.contains(typeName)) {
                error(s"Type $typeName already exists in the current environment")
              }

              // Check if the type definition is well-formed
              if (!isWellFormed(TypeDef(typeName, typeParams, w), accEnv)) {
                error(s"Type definition of $typeName is not well-formed")
              }

              // Return new environment with the type definition
              accEnv + (typeName -> Right(TypeDef(typeName, typeParams, w)))
            case _ => error("Expected a type definition")
          }
        }
        val finalEnv = defs.foldLeft(tenv)(processDef)
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
