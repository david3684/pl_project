file://<WORKSPACE>/proj3/core/src/main/scala/cs320/Implementation.scala
### file%3A%2F%2F%2FUsers%2Ftreblocami%2FDesktop%2Fjob%2Fpl_project%2Fproj3%2Fcore%2Fsrc%2Fmain%2Fscala%2Fcs320%2FImplementation.scala:126: error: Invalid literal number
          case (IntV(l), IntV(r)) => if(r!=0IntV(l%r)
                                           ^

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
      case Mul(l,r) => 
        val lv = interpHelper(l, env)
        val rv = interpHelper(r, env)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => IntV(l*r)
          case _ => error("Not Integer")
        }
      case Div(l,r) => 
        val lv = interpHelper(l, env)
        val rv = interpHelper(r, env)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => IntV(l/r)
          case _ => error("Not Integer")
        }
      case Mod(l,r) => 
        val lv = interpHelper(l, env)
        val rv = interpHelper(r, env)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => if(r!=0IntV(l%r)
          case _ => error("Not Integer")
        }
      case Eq(l,r) => 
        val lv = interpHelper(l, env)
        val rv = interpHelper(r, env)
        (lv, rv) match{
          case (IntV(l), IntV(r)) => if(l==r) BooleanV(true) else BooleanV(false)
          case _ => error("Not Integer")
        }
      case Untyped.Sequence(l, r) =>
        interpHelper(l, env)
        val rv = interpHelper(r, env)
        rv
    }
  }
}

```



#### Error stacktrace:

```
scala.meta.internal.tokenizers.Reporter.syntaxError(Reporter.scala:23)
	scala.meta.internal.tokenizers.Reporter.syntaxError$(Reporter.scala:23)
	scala.meta.internal.tokenizers.Reporter$$anon$1.syntaxError(Reporter.scala:33)
	scala.meta.internal.tokenizers.Reporter.syntaxError(Reporter.scala:25)
	scala.meta.internal.tokenizers.Reporter.syntaxError$(Reporter.scala:25)
	scala.meta.internal.tokenizers.Reporter$$anon$1.syntaxError(Reporter.scala:33)
	scala.meta.internal.tokenizers.LegacyScanner.checkNoLetter(LegacyScanner.scala:820)
	scala.meta.internal.tokenizers.LegacyScanner.restOfUncertainToken$1(LegacyScanner.scala:864)
	scala.meta.internal.tokenizers.LegacyScanner.getNumber(LegacyScanner.scala:872)
	scala.meta.internal.tokenizers.LegacyScanner.fetchZero$1(LegacyScanner.scala:330)
	scala.meta.internal.tokenizers.LegacyScanner.fetchToken(LegacyScanner.scala:332)
	scala.meta.internal.tokenizers.LegacyScanner.nextToken(LegacyScanner.scala:211)
	scala.meta.internal.tokenizers.LegacyScanner.foreach(LegacyScanner.scala:1011)
	scala.meta.internal.tokenizers.ScalametaTokenizer.uncachedTokenize(ScalametaTokenizer.scala:24)
	scala.meta.internal.tokenizers.ScalametaTokenizer.$anonfun$tokenize$1(ScalametaTokenizer.scala:17)
	scala.collection.concurrent.TrieMap.getOrElseUpdate(TrieMap.scala:962)
	scala.meta.internal.tokenizers.ScalametaTokenizer.tokenize(ScalametaTokenizer.scala:17)
	scala.meta.internal.tokenizers.ScalametaTokenizer$$anon$2.apply(ScalametaTokenizer.scala:332)
	scala.meta.tokenizers.Api$XtensionTokenizeDialectInput.tokenize(Api.scala:25)
	scala.meta.tokenizers.Api$XtensionTokenizeInputLike.tokenize(Api.scala:14)
	scala.meta.internal.parsers.ScannerTokens$.apply(ScannerTokens.scala:912)
	scala.meta.internal.parsers.ScalametaParser.<init>(ScalametaParser.scala:33)
	scala.meta.parsers.Parse$$anon$1.apply(Parse.scala:35)
	scala.meta.parsers.Api$XtensionParseDialectInput.parse(Api.scala:25)
	scala.meta.internal.semanticdb.scalac.ParseOps$XtensionCompilationUnitSource.toSource(ParseOps.scala:17)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument.toTextDocument(TextDocumentOps.scala:206)
	scala.meta.internal.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:54)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$semanticdbTextDocument$1(ScalaPresentationCompiler.scala:356)
```
#### Short summary: 

file%3A%2F%2F%2FUsers%2Ftreblocami%2FDesktop%2Fjob%2Fpl_project%2Fproj3%2Fcore%2Fsrc%2Fmain%2Fscala%2Fcs320%2FImplementation.scala:126: error: Invalid literal number
          case (IntV(l), IntV(r)) => if(r!=0IntV(l%r)
                                           ^