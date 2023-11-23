file://<WORKSPACE>/proj3/core/src/main/scala/cs320/Implementation.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
uri: file://<WORKSPACE>/proj3/core/src/main/scala/cs320/Implementation.scala
text:
```scala
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
    def makeTypeEnv(d: RecDef, tenv: TypeEnv): TypeEnv = d match{
      case Lazy(name, typ, expr) => {
        val newtenv = tenv + (name -> Left((TypeScheme(List(), typ), true)))
        newtenv
      }
      case RecFun(name, tparams, params, rtype, body) => {
        val paramTypes = params.map(_._2)
        val funcTypeScheme = TypeScheme(tparams, ArrowT(paramTypes, rtype))
        val newtenv = tenv + (name -> Left((funcTypeScheme, true)))
        newtenv
      }
      case TypeDef(name, tparams, variants) => {
        // First, add the TypeDef itself to the environment
        val initialEnv = tenv + (name -> Right(TypeDef(name, tparams, variants)))

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
            env + (variantName -> Left((variantType, true))) // Assuming 'val' as true
        }

        updatedEnv
      }
    }
    def isRecWellFormed(t: RecDef, tenv: TypeEnv): Boolean = t match{
      case TypeDef(name, tparams, variants) =>
        if (tparams.forall(param => !tenv.contains(param))) {
          val updatedEnv = tparams.foldLeft(tenv) {
            (env, param) => env + (param -> Left((TypeScheme(List(), VarT(param)), false)))
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
          if(typ == helperTypeCheck(expr, tenv)) true
          else false
        }
        else false

      case RecFun(name, tparams, params, rtype, body) => 
        if (tparams.forall(param => !tenv.contains(param))){
          val updatedEnv = tparams.foldLeft(tenv)((env, param) => env + (param -> Left((TypeScheme(List(), VarT(param)), false))))
          if(params.forall(param => isWellFormed(param._2, updatedEnv))){
            if(isWellFormed(rtype, updatedEnv)){
              val finalEnv = params.foldLeft(updatedEnv) {
                (env, param) => env + (param._1 -> Left((TypeScheme(List(), param._2), true)))
              }
              val bodyType = helperTypeCheck(body, finalEnv)
              if (bodyType == rtype) {
                true
              }
              else false
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
        val finalEnv = defs.foldLeft(tenv) { (accEnv, d) =>
          d match {
            case TypeDef(typeName, typeParams, _) =>
              if (accEnv.contains(typeName)) {
                error(s"Type $typeName already exists in the environment")
              }  // Replace ??? with the appropriate type or body of the type definition
              else {
                val emptytenv = Map.empty[String, Either[(TypeScheme, Mutability), TypeDef]]
                val dtypeenv = makeTypeEnv(d, emptytenv)
                accEnv ++ dtypeenv
              }
            case _ => 
              val emptytenv = Map.empty[String, Either[(TypeScheme, Mutability), TypeDef]]
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
    type Sto = Map[Addr, Option[Value]]
    type Env = Map[String, Addr]
    def malloc(sto: Sto): Addr = (sto.keySet + 0).max + 1
    /*def strict(v: Value, s: Sto): Value = v match {
      case ExprV(e, env) => strict(interpHelper(e, env, s))
      case _ => v
    }*/
    def interp(expr: Expr): Value = interpHelper(expr, Map(), Map()) match{
      case (Some(v), _) => v
    }
    def interpHelper(e: Expr, env: Env, sto: Sto):(Option[Value], Sto) = e match{ 
      case IntE(n) => (Some(IntV(n)), sto)
      case BooleanE(b) => (Some(BooleanV(b)), sto)
      case Id(x) => 
        val addr = env.getOrElse(x, error(s"free identifier: $x"))
        val optXv = sto.getOrElse(addr, None) // None 반환으로 변경
        optXv match {
          case Some(ExprV(e, xenv)) => {
            val (ev, es) = interpHelper(e, xenv, sto)
            (ev, es + (addr -> ev))
          }
          case Some(otherValue) => (Some(otherValue), sto)
          case None => error(s"Value not found for identifier: $x")
        }
      case Add(l,r) => 
        val ((lv, ls)) = interpHelper(l, env, sto)
        val ((rv, rs)) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (Some(IntV(l)), Some(IntV(r))) => (Some(IntV(l+r)), sto)
          case _ => error("Not Integer")
        }
      case Mul(l,r) => 
        val (lv, ls) = interpHelper(l, env, sto)
        val ((rv, rs)) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (Some(IntV(l)), Some(IntV(r))) => (Some(IntV(l*r)), sto)
          case _ => error("Not Integer")
        }
      case Div(l,r) => 
        val (lv, ls) = interpHelper(l, env, sto)
        val (rv, rs) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (Some(IntV(l)), Some(IntV(r))) => if(r!=0) (Some(IntV(l/r)),sto) else error("Division by Zero")
          case _ => error("Not Integer")
        }
      case Mod(l,r) => 
        val (lv, ls) = interpHelper(l, env, sto)
        val (rv, rs) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (Some(IntV(l)), Some(IntV(r))) => if(r!=0) (Some(IntV(l%r)),sto) else error("Division by Zero")
          case _ => error("Not Integer")
        }
      case Eq(l,r) => 
        val (lv, ls) = interpHelper(l, env, sto)
        val (rv, rs) = interpHelper(r, env, sto)
        (lv, rv) match{
          case (Some(IntV(l)), Some(IntV(r))) => if(l==r) (Some(BooleanV(true)),sto) else (Some(BooleanV(false)), sto)
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
      case Fun(params, body) => {
        (Some(CloV(params, body, env)), sto)
      }
      case Assign(name, expr) => {
        val naddr = env.getOrElse(name, error(s"Free Identifier: $name"))
        val (ev, es) = interpHelper(expr, env, sto)
        val newsto = es + (naddr -> ev)
        (None, newsto)
      }
      case App(fun, args) => {
        val (ev, es) = interpHelper(fun, env, sto)
        ev match {
          case Some(clo: CloV) => {
            if (args.length != clo.params.length) {
              error("Incorrect number of arguments for closure")
            } else {
              val newEnv = clo.env
              val newSto = args.foldLeft(es) { (accSto, arg) =>
                val (argVal, updatedSto) = interpHelper(arg, env, accSto)
                val newAddr = malloc(updatedSto) // A function to generate a new address
                updatedSto + (newAddr -> argVal)
              }
              interpHelper(clo.body, newEnv, newSto)
            }
          }
          case Some(ConstructorV(name)) => {
            val argsVals = args.map(arg => interpHelper(arg, env, sto)._1)
            argsVals.foldLeft(Option(List.empty[Value])) { (acc, optVal) =>
              for {
                accList <- acc
                value <- optVal
              } yield accList :+ value
            } match {
              case Some(values) => (Some(VariantV(name, values)), es)
              case None => error("One of the arguments is not a value")
            }
          }
          case _ => error("Not either Closure or Constructor")
        }
      }
      case Match(expr, cases) => {
        (ev, es)
      }
    }
  }
}

```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2582)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.isSelfSym(SymDenotations.scala:714)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:160)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1627)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1629)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1660)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:281)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1668)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:281)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1627)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1629)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1666)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:281)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$13(ExtractSemanticDB.scala:221)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:221)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1711)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:184)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1627)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1629)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1660)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:281)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1668)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:281)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1627)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1629)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1666)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:281)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1715)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:184)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$11(ExtractSemanticDB.scala:207)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:207)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1719)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:181)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$11(ExtractSemanticDB.scala:207)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:207)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1761)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1719)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1633)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1762)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:181)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$1(ExtractSemanticDB.scala:145)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:145)
	scala.meta.internal.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:38)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticdbTextDocument$$anonfun$1(ScalaPresentationCompiler.scala:178)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner