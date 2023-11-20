file://<WORKSPACE>/proj3/core/src/main/scala/cs320/Implementation.scala
### java.lang.NullPointerException: Cannot invoke "scala.reflect.internal.Symbols$Symbol.isModule()" because the return value of "scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.methodSym()" is null

occurred in the presentation compiler.

action parameters:
offset: 3468
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
        
    }
  }

  object U {
    import Untyped._

    def interp(expr: Expr): Value = interpHelper(expr, Map())
    def interpHelper(e: Expr, env: Map[String, Value]):Value = e match{
      case IntE(n) => IntV(n)
      case BooleanE(b) => BooleanV(b)
      case Add(l,r) => 
        val lv = interpHelper(l, env)
        val rv = interpHelper(r, env)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => IntV(l+r)
          case _ => error("Not Integer")
        }
      case Untyped.Sequence(l, r) =>
        interpHelper(l, env)
        rv = interpHelper(r@@)
    }
  }
}

```



#### Error stacktrace:

```
scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.methodsParams$lzycompute(ArgCompletions.scala:30)
	scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.methodsParams(ArgCompletions.scala:29)
	scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.allParams$lzycompute(ArgCompletions.scala:81)
	scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.allParams(ArgCompletions.scala:81)
	scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.params$lzycompute(ArgCompletions.scala:83)
	scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.params(ArgCompletions.scala:82)
	scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.contribute(ArgCompletions.scala:210)
	scala.meta.internal.pc.CompletionProvider.expected$1(CompletionProvider.scala:439)
	scala.meta.internal.pc.CompletionProvider.safeCompletionsAt(CompletionProvider.scala:499)
	scala.meta.internal.pc.CompletionProvider.completions(CompletionProvider.scala:58)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$complete$1(ScalaPresentationCompiler.scala:169)
```
#### Short summary: 

java.lang.NullPointerException: Cannot invoke "scala.reflect.internal.Symbols$Symbol.isModule()" because the return value of "scala.meta.internal.pc.completions.ArgCompletions$ArgCompletion.methodSym()" is null