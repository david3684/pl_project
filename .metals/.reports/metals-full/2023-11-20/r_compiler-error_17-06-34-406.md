file://<WORKSPACE>/proj3/core/src/main/scala/cs320/Implementation.scala
### java.lang.RuntimeException: ambiguous mend file%3A%2F%2F%2FUsers%2Ftreblocami%2FDesktop%2Fjob%2Fpl_project%2Fproj3%2Fcore%2Fsrc%2Fmain%2Fscala%2Fcs320%2FImplementation.scala@263..263  file%3A%2F%2F%2FUsers%2Ftreblocami%2FDesktop%2Fjob%2Fpl_project%2Fproj3%2Fcore%2Fsrc%2Fmain%2Fscala%2Fcs320%2FImplementation.scala@252..263 isWellTyped

occurred in the presentation compiler.

action parameters:
uri: file://<WORKSPACE>/proj3/core/src/main/scala/cs320/Implementation.scala
text:
```scala
package cs320

import scala.runtime.RichBoolean

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)

  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._
    def isWellTyped
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
    }
  }
}

```



#### Error stacktrace:

```
scala.sys.package$.error(package.scala:27)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.indexName(TextDocumentOps.scala:98)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.apply(TextDocumentOps.scala:195)
	scala.meta.transversers.Traverser.applyRest(Traverser.scala:4)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.apply(TextDocumentOps.scala:202)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.transversers.Traverser.applyRest(Traverser.scala:4)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.apply(TextDocumentOps.scala:202)
	scala.meta.transversers.Traverser.applyDefn(Traverser.scala:4)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.apply(TextDocumentOps.scala:202)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.transversers.Traverser.applyRest(Traverser.scala:4)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.apply(TextDocumentOps.scala:202)
	scala.meta.transversers.Traverser.applyDefn(Traverser.scala:4)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.apply(TextDocumentOps.scala:202)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.transversers.Traverser.applyRest(Traverser.scala:4)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.apply(TextDocumentOps.scala:202)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.transversers.Traverser.applyRest(Traverser.scala:4)
	scala.meta.transversers.Traverser.apply(Traverser.scala:4)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument$traverser$1$.apply(TextDocumentOps.scala:202)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument.toTextDocument(TextDocumentOps.scala:206)
	scala.meta.internal.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:54)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$semanticdbTextDocument$1(ScalaPresentationCompiler.scala:356)
```
#### Short summary: 

java.lang.RuntimeException: ambiguous mend file%3A%2F%2F%2FUsers%2Ftreblocami%2FDesktop%2Fjob%2Fpl_project%2Fproj3%2Fcore%2Fsrc%2Fmain%2Fscala%2Fcs320%2FImplementation.scala@263..263  file%3A%2F%2F%2FUsers%2Ftreblocami%2FDesktop%2Fjob%2Fpl_project%2Fproj3%2Fcore%2Fsrc%2Fmain%2Fscala%2Fcs320%2FImplementation.scala@252..263 isWellTyped