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
    
    sealed trait TypeKey
    case class SchemeKey(identifier: String) extends TypeKey
    case class DefKey(typeName: String) extends TypeKey
    case class VarKey(typeVar: String) extends TypeKey
    
    type TypeEnv = Map[TypeKey, Either[Either[(TypeScheme, Mutability), TypeDef], VarT]]
    def caseTypeCheck(variants: List[Variant], tparams: List[String], types: List[Type], cvar: String, cnames: List[String], body: Expr, tenv: TypeEnv): Type = {
      if(tparams.length != types.length) error("Number of parameters and types doesn't match")
      val answer = variants.find(_.name == cvar)
      answer match{
        case Some(Variant(vname, vparams)) => {
          if(vparams.length!=cnames.length) error("ERROR!!")
          val substitutedVparams = vparams.map(vp => substituteType(vp, tparams, types))
          val newEnv = cnames.zip(substitutedVparams).foldLeft(tenv) {
            case (acctenv, (name, typ)) => acctenv + (SchemeKey(name) -> Left(Left(TypeScheme(List(), typ), false)))
          }
          helperTypeCheck(body, newEnv)
        }
        case _ => error("ERROR")
      }
    }
    def makeTypeEnv(d: RecDef, tenv: TypeEnv): TypeEnv = d match{
      case Lazy(name, typ, expr) => {
        val newtenv = tenv + (SchemeKey(name) -> Left(Left((TypeScheme(List(), typ), true))))
        newtenv
      }
      case RecFun(name, tparams, params, rtype, body) => {
        val paramTypes = params.map(_._2)
        val funcTypeScheme = TypeScheme(tparams, ArrowT(paramTypes, rtype))
        tenv + (SchemeKey(name) -> Left(Left((funcTypeScheme, true))))
      }
      case TypeDef(name, tparams, variants) => {
        // First, add the TypeDef itself to the environment
        val initialEnv = tenv + (DefKey(name) -> Left(Right(TypeDef(name, tparams, variants))))
        // Process each variant and update the environment
        val updatedEnv = variants.foldLeft(initialEnv) {
          case (env, Variant(variantName, types)) =>
            val variantType = if (types.isEmpty) {
              // If the variant does not take any parameter
              TypeScheme(tparams, AppT(name, tparams.map(VarT)))
            } else {
              // If the variant takes one or more parameters
              val funcType = ArrowT(types, AppT(name, tparams.map(VarT)))
              TypeScheme(tparams, funcType)
            }
            // Add a mapping for the variant to the environment
            env + (SchemeKey(variantName) -> Left(Left((variantType, true)))) // Assuming 'val' as true
        }
        updatedEnv
      }
    }
    def isRecWellFormed(t: RecDef, tenv: TypeEnv): Boolean = t match{
      case TypeDef(name, tparams, variants) =>
        if (tparams.forall(param => !tenv.contains(VarKey(param)))) {
          val updatedEnv = tparams.foldLeft(tenv) {
            (env, param) => env + (VarKey(param) -> Right(VarT(param)))
          }
          
          val allVariantsWellFormed = variants.forall { case Variant(variantName, types) =>
            types.forall(typ => isWellFormed(typ, updatedEnv))
          }
          allVariantsWellFormed
        } else {
          false
        }

      case Lazy(name, typ, expr) => 
        if(isWellFormed(typ, tenv)) {
          typ == helperTypeCheck(expr, tenv)
        }
        else false


      case RecFun(name, tparams, params, rtype, body) => 
        if (tparams.forall(param => !tenv.contains(SchemeKey(param)))){
          val updatedEnv = tparams.foldLeft(tenv)((env, param) => env + (SchemeKey(param) -> Left(Left((TypeScheme(List(), VarT(param)), false)))))
          if(params.forall(param => isWellFormed(param._2, updatedEnv))){
            if(isWellFormed(rtype, updatedEnv)){
              val finalEnv = params.foldLeft(updatedEnv) {
                (env, param) => env + (SchemeKey(param._1) -> Left(Left((TypeScheme(List(), param._2), true))))
              }
              helperTypeCheck(body, finalEnv) == rtype
            }
            else false
          }
          else false
        }
        else false
    }
    def isWellFormed(t: Type, tenv: TypeEnv): Boolean = t match{
      case IntT => true
      case BooleanT => true
      case UnitT => true
      case AppT(name, targs) => 
        if(targs.forall(arg => isWellFormed(arg, tenv))) {
          if(tenv.contains(DefKey(name))) {
            val nt = tenv.getOrElse(DefKey(name),false)
            nt match{
              case Left(Right(TypeDef(_,tparams, _))) => tparams.length == targs.length
              case _ => false
            }
          }
          else false
        }
        else false

      case VarT(a) => 
        if(tenv.contains(SchemeKey(a))) true
        else false

      case ArrowT(ptypes, rtype) => 
        if(ptypes.forall(arg => isWellFormed(arg, tenv))) {
          if(isWellFormed(rtype, tenv)) {
            true
          }
          else false
        }
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

    def typeCheck(expr: Expr): Type = helperTypeCheck(expr, Map.empty[TypeKey, Either[Either[(TypeScheme, Mutability), TypeDef], VarT]])
    def helperTypeCheck(expr: Expr, tenv: TypeEnv): Type = expr match{
      case IntE(_) => IntT
      case BooleanE(_) => BooleanT
      case UnitE => UnitT
      case Id(x, types) => 
        types match{
          case List() => {
            val Left(Left((xt, mut))) = tenv.getOrElse(SchemeKey(x), error(s"No type for identifier: $x"))
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
              val Left(Left((xt, mut))) = tenv.getOrElse(SchemeKey(x), error(s"No type for identifier: $x"))
              xt match {
                case TypeScheme(tvars, t) => {
                  if (tvars.length != types.length) error("Error")

                  if(tvars.length == 0) t
                  else {
                    substituteType(t, tvars, types)
                  }
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
                  val newtenv = tenv + (SchemeKey(name) -> Left(Left((TypeScheme(List(), et), mut))))
                  helperTypeCheck(b, newtenv)
                }
                else error("ERROR")    
              }
              case false => error("Not Well-Formed Type")
            }
          }
          case None => {
            val et = helperTypeCheck(e, tenv)
            val newtenv = tenv + (SchemeKey(name) -> Left(Left((TypeScheme(List(), et), mut))))
            helperTypeCheck(b, newtenv)
          }
        }
      case Fun(params, b) => {
        if(params.forall(arg => isWellFormed(arg._2, tenv))){
          val updatedEnv = params.foldLeft(tenv) { (accEnv, param) =>
            accEnv + (SchemeKey(param._1) -> Left(Left((TypeScheme(List(), param._2), true)))) // Assuming true for val (non-mutable)
          }
          val bodyType = helperTypeCheck(b, updatedEnv)
          val funcType = ArrowT(params.map(_._2), bodyType)
          funcType
        }
        else error("Some Types are Not Well-Formed")
      }
      case Assign(name, expr) => {
        val Left(Left((xt, mut))) = tenv.getOrElse(SchemeKey(name), error(s"No type for identifier: $name"))
        xt match{
          case TypeScheme(typevars, t) => if(typevars.length == 0){
            if(mut){
              val et = helperTypeCheck(expr, tenv)
              if(et==t) UnitT
              else error("ERROR")
            }
            else error("Immutable")
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
            tenv.get(DefKey(typeName)) match {
              case Some(Left(Right(TypeDef(name, typeParams, variants)))) =>
                if (typeArgs.length != typeParams.length) error("Number of typeargs and typeparams mismatch")
                if (cases.length != variants.length) error("Number of cases and variants mismatch")
                val caseTypes = cases.map { caseObj =>
                  caseTypeCheck(variants, typeParams, typeArgs, caseObj.variant, caseObj.names, caseObj.body, tenv)
                }
                if (caseTypes.forall(_ == caseTypes.head)) caseTypes.head
                else error("All cases must have the same type")
              case _ => error("Type application expected")
            }
          case _ => error("Type application expected in match expression")
        }
      }
      case RecBinds(defs, body) => {
        val finalEnv = defs.foldLeft(tenv) { (accEnv, d) =>
          d match {
            case TypeDef(typeName, typeParams, _) =>
              if (tenv.contains(DefKey(typeName))) {
                error(s"Type $typeName already exists in the environment")
              }  
              else {
                val emptytenv = Map.empty[TypeKey, Either[Either[(TypeScheme, Mutability), TypeDef], VarT]]
                val dtypeenv = makeTypeEnv(d, emptytenv)
                accEnv ++ dtypeenv
              }
            case _ => 
              val emptytenv = Map.empty[TypeKey, Either[Either[(TypeScheme, Mutability), TypeDef], VarT]]
              val dtypeenv = makeTypeEnv(d, emptytenv)
              accEnv ++ dtypeenv
          }
        }
        if (defs.exists(d => !isRecWellFormed(d, finalEnv))) {
          error("One or more definitions are not well-formed")
        }
        val bt = helperTypeCheck(body, finalEnv)
        if(!isWellFormed(bt, tenv)){
          error("Final type is not well-formed")
        }
        else{
          bt
        }
      }
    }
  }

  object U {
    import Untyped._
    type Sto = Map[Addr, Value]
    type Env = Map[String, Addr]
    def malloc(sto: Sto): Addr = (sto.keySet + 0).max + 1
    def envmalloc(env: Env): Addr = {
      if(env.isEmpty) 1
      else env.values.max+1
    }
    def strict(vSto: (Value, Sto)): (Value, Sto) = vSto match {
      case (ExprV(e, env), s) =>{
        strict(interpHelper(e, env, s))
      } 
      case _ => vSto
    }
    def createEnv(expr: RecDef, env: Env, sto:Sto): Env = expr match{
      case Lazy(name, _) => {
        val emptyenv = Map.empty[String, Addr]
        emptyenv + (name -> envmalloc(env))
      }
      case RecFun(name, params, body) => {
        val emptyenv = Map.empty[String, Addr]
        emptyenv + (name -> envmalloc(env))
      }
      case TypeDef(variants) => {
        variants.foldLeft(env){ (accEnv, v) =>
          v match {
            case Variant(name, _) => {
              accEnv + (name -> envmalloc(accEnv))
            }
            case _ => error("ERROR")
          }
          
        }
      }
    }
    def createSto(expr: RecDef, env: Env, sto:Sto): Sto = expr match{
      case Lazy(name, e) => {
        val addr = env.getOrElse(name, error("No Value for address"))
        sto + (addr -> ExprV(e, env))
      }
      case RecFun(name, params, body) => {
        val addr = env.getOrElse(name, error("No Value for address"))
        sto + (addr -> CloV(params, body, env))
      }
      case TypeDef(variants) => {
        variants.foldLeft(sto){ (accSto, v) =>
          v match{
            case Variant(name, true) => {
              val addr = env.getOrElse(name, error("No Value for address")) 
              accSto + (addr -> VariantV(name, List()))
            }
            case Variant(name, false) => {
              val addr = env.getOrElse(name, error("No Value for address")) 
              accSto + (addr -> ConstructorV(name))
            }
            case _ => error("ERROR")
          }
        }
      }
    }
    def interp(expr: Expr): Value = interpHelper(expr, Map(), Map()) match{
      case ((v), _) => v
    }
    def interpHelper(e: Expr, env: Env, sto: Sto):(Value, Sto) = e match{ 
      case IntE(n) => (IntV(n), sto)
      case BooleanE(b) => ((BooleanV(b)), sto)
      case UnitE => (UnitV, sto)
      case Id(x) => 
        val addr = env.getOrElse(x, error(s"free identifier: $x"))
        val xv = sto.getOrElse(addr, error(s"No Value Mapped to Address: $addr")) // None 반환으로 변경
        xv match {
          case (ExprV(e, xenv)) => {
            val (ev, es) = strict(interpHelper(e, xenv, sto))
            (ev, es + (addr -> ev))
          }
          case _ => (xv, sto)
        }
      case Add(l,r) => 
        val ((lv, ls)) = strict(interpHelper(l, env, sto))
        val ((rv, rs)) = strict(interpHelper(r, env, ls))
        (lv, rv) match{
          case ((IntV(l)), (IntV(r))) => ((IntV(l+r)), rs)
          case _ => error("Not Integer")
        }
      case Mul(l,r) => 
        val (lv, ls) = strict(interpHelper(l, env, sto))
        val ((rv, rs)) = strict(interpHelper(r, env, ls))
        (lv, rv) match{
          case ((IntV(l)), (IntV(r))) => ((IntV(l*r)), rs)
          case _ => error("Not Integer")
        }
      case Div(l,r) => 
        val (lv, ls) = strict(interpHelper(l, env, sto))
        val (rv, rs) = strict(interpHelper(r, env, ls))
        (lv, rv) match{
          case ((IntV(l)), (IntV(r))) => if(r!=0) ((IntV(l/r)), rs) else error("Division by Zero")
          case _ => error("Not Integer")
        }
      case Mod(l,r) => 
        val (lv, ls) = strict(interpHelper(l, env, sto))
        val (rv, rs) = strict(interpHelper(r, env, ls))
        (lv, rv) match{
          case ((IntV(l)), (IntV(r))) => if(r!=0) ((IntV(l%r)), rs) else error("Division by Zero")
          case _ => error("Not Integer")
        }
      case Eq(l,r) => 
        val (lv, ls) = strict(interpHelper(l, env, sto))
        val (rv, rs) = strict(interpHelper(r, env, ls))
        (lv, rv) match{
          case ((IntV(l)), (IntV(r))) => if(l==r) ((BooleanV(true)), rs) else ((BooleanV(false)), rs)
          case _ => error("Not Integer")
        }
      case Lt(l, r) =>
        val (lv, ls) = strict(interpHelper(l, env, sto))
        val (rv, rs) = strict(interpHelper(r, env, ls))
        (lv, rv) match{
          case ((IntV(l)), (IntV(r))) => if(l<r) ((BooleanV(true)), rs) else ((BooleanV(false)), rs)
          case _ => error("Not Integer")
        }
      case Untyped.Sequence(l, r) =>
        val (lv, ls) = strict(interpHelper(l, env, sto))
        val (rv, rs) = strict(interpHelper(r, env, ls))
        (rv,rs)
      case Val(name, e, b) => {
        val (ev, es) = strict(interpHelper(e,env,sto))
        val addr = malloc(es)
        strict(interpHelper(b, env + (name->addr), es + (addr -> ev)))
      }
      case Fun(params, body) => {
        ((CloV(params, body, env)), sto)
      }
      case Assign(name, expr) => {
        val naddr = env.getOrElse(name, error(s"Free Identifier: $name"))
        val (ev, es) = strict(interpHelper(expr, env, sto))
        val newsto = es + (naddr -> ev) //?????
        (UnitV, newsto)
      }
      case If(cond, tbranch, fbranch) => {
        val (cv, cs) = strict(interpHelper(cond, env, sto))
        cv match {
          case (BooleanV(true)) => {
            strict(interpHelper(tbranch, env, cs))
          }
          case (BooleanV(false)) => {
            strict(interpHelper(fbranch, env, cs))
          }
          case _ => error("Condition is not boolean")
        }
      }
      case RecBinds(defs, body) => {
        val sumEnv = defs.foldLeft(env) { (accEnv, d) =>
          accEnv ++ createEnv(d, accEnv, sto)
        }

        val finalSto = defs.foldLeft(sto) { (accSto, d) =>
          accSto ++ createSto(d, sumEnv, accSto)
        }
        strict(interpHelper(body, sumEnv, finalSto))
      }
      case App(fun, args) => {
        val (ev, es) = strict(interpHelper(fun, env, sto))
        ev match {
          case CloV(params, body, fenv) =>
            if (args.length != params.length) {
              error("Incorrect number of arguments for closure")
            } else {
              val (newEnv, newSto) = params.zip(args).foldLeft((fenv, es)) {
                case ((accEnv, accSto), (param, arg)) =>
                  val (argVal, updatedSto) = strict(interpHelper(arg, env, accSto))
                  val newAddr = malloc(updatedSto) // 새 주소 생성
                  (accEnv + (param -> newAddr), updatedSto + (newAddr -> argVal))
              }
              interpHelper(body, newEnv, newSto)
            }

          case ConstructorV(name) => {
            val argsVals = args.map(arg => interpHelper(arg, env, sto)._1)
            (VariantV(name, argsVals), es)
          }
          case _ => error("Not either Closure or Constructor")
        }
      }

      case Match(expr, cases) => {
        val (ev, es) = strict(interpHelper(expr, env, sto))
        ev match {
          case VariantV(name, values) =>
            cases.find(_.variant == name) match {
              case Some(Case(cname, names, body)) if names.length == values.length =>
                val (newEnv, newSto) = (names zip values).foldLeft((env, es)) {
                  case ((accEnv, accSto), (name, value)) =>
                    val newAddr = malloc(accSto) // A function to generate a new address
                    val updatedEnv = accEnv + (name -> newAddr)
                    val updatedSto = accSto + (newAddr -> value) // Directly using value
                    (updatedEnv, updatedSto)
                }
                val (vc, vs) = strict(interpHelper(body, newEnv, newSto))
                (vc, vs)
              case _ => error("No Matching Case")
            }
          case _ => error("Not a Variant")
        }
      }
    }
  }
}
