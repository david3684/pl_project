package cs320

import Value._

object Implementation extends Template {
  def numVop(op: (Int,Int) => Int): (Expr, Expr) => Expr = (_,_) match{
    case (Num(x), Num(y)) => Num(op(x,y))
    case (x,y) => error(s"not both numbers: $x, $y")
  } 


  val numVadd = numVop(_+_)
  val numVsub = numVop(_-_)
  def interp(expr: Expr, env: Env): Value = expr match{
    case Num(n) => NumV(n)
    case Add(l,r) => numVadd(interp(l,env), interp(r,env))
    case Sub(l,r) => numVsub(interp(l,env), interp(r,env))
    case Val(x,e,b) => interp(b, env + (x -> interp(e, env)))
    case Id(x) => env.getOrElse(x, error(s"free identifier: $x"))
    case Fun(ps,b) => CloV(ps, b, env)
    case App(f, args) => interp(f,env) match{
      case CloV(params, b, fEnv) => {
        val avals = args.map(interp(_,env))
        if (args.length != params.length) error(s"wrong arity") //  변수 개수가 안맞아
        interp(b, fenv ++ (params zip avals))
      }
      case v => error(s"Not a closure")
    }
    case Rec(m) => RecV(m.map { case(f,e) => (f,interp(e,env))})
    case Acc(r, x) => interp(r, env) match {
      case RecV(m) => m.getOrElse(x, error(s"no such field: $x"))
      case v => error(s"not a record: $v")
    }
  }
  def interp(expr: Expr): Value = interp(expr, Map())
}
