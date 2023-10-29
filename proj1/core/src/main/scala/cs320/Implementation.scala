package cs320

import Value._

object Implementation extends Template {
  def binaryOperation(left: Expr, right: Expr, env: Map[String, Value], operation: (BigInt, BigInt) => Value, errorMessage: String): Value = {
    val leftVal = helper(left, env)
    val rightVal = helper(right, env)
    (leftVal, rightVal) match {
      case (IntV(x), IntV(y)) => operation(x,y)
      case _ => error(errorMessage)

    }
  }
  

  def interp(expr: Expr): Value = helper(expr, Map())

  def helper(expr: Expr, env: Map[String, Value]): Value = expr match{
    case NilE => NilV
    case IntE(n) => IntV(n)
    case Id(x) => env.getOrElse(x, error(s"free identifier: $x"))
    case BooleanE(b) => BooleanV(b)

    case Add(left, right) => binaryOperation(left, right, 
    env, (x, y) => IntV(x + y), "Both operands must be integers for addition.")
    case Mul(left, right) => binaryOperation(left, right, 
    env, (x, y) => IntV(x * y), "Both operands must be integers for multiplication.")
    case Eq(left, right) => binaryOperation(left, right, 
    env, (x, y) => BooleanV(x == y), "Both operands must be integers for equality.")
    case Lt(left, right) => binaryOperation(left, right, 
    env, (x, y) => BooleanV(x < y), "Both operands must be integers for equality.")
    
    case Mod(left, right) => 
      val leftVal = helper(left, env)
      val rightVal = helper(right, env)
      (leftVal, rightVal) match {
        case (IntV(x), IntV(y)) => 
          if(y!=0) IntV(x%y)
          else error(s"Can't divide by zero")
        case _ => error(s"Both operands should be integer")
      }

    case Div(left, right) => 
      val leftVal = helper(left, env)
      val rightVal = helper(right, env)
      (leftVal, rightVal) match {
        case (IntV(x), IntV(y)) => 
          if(y!=0) IntV(x/y)
          else error(s"Can't divide by zero")
        case _ => error(s"Both operands should be integer")
      }
    
    case If(condition: Expr, trueBranch: Expr, falseBranch: Expr) => helper(condition, env) match{
      case BooleanV(true) => helper(trueBranch, env)
      case BooleanV(false) => helper(falseBranch, env)
      case _ => error(s"Condition should return a boolean value")
    }

    case TupleE(exprL) => 
      val valL = exprL.map(expr => helper(expr, env))
      TupleV(valL)

    case Proj(l, index) => 
      val valL = helper(l, env)
      valL match{
        case TupleV(values) => 
          if(index>=1 && index <= values.length) values(index-1)
          else error(s"Index out of bound")
        case _ => error(s"Projection can be applied only for tuples")
      }

    case ConsE(head, tail) =>   
      val headV = helper(head, env)
      val tailV = helper(tail, env)
      (headV, tailV) match{
        case (_, NilV) => ConsV(headV, NilV)
        case (_, ConsV(_,_)) => ConsV(headV, tailV)
        case _ => error(s"Second Argument should be a list")
      }

    case Empty(l) =>
      val listVal = helper(l, env)
      listVal match{
        case NilV => BooleanV(true)
        case ConsV(_,_) => BooleanV(false)
        case _ => error(s"Given expr is not a list")
      }

    case Head(l) => 
      val listVal = helper(l, env)
      listVal match{
        case ConsV(head,_) => head
        case _ => error(s"Given expr is not a list")
      }

    case Tail(l) => 
      val listVal = helper(l, env)
      listVal match{
        case ConsV(_,tail) => tail
        case _ => error(s"Given expr is not a list")
      }

    case Val(n, e, b) => helper(b, env+(n->helper(e,env)))

    case Fun(params, b) => CloV(params, b, env)


    case RecFuns(f: List[FunDef], b: Expr) => 
      
      val Closures = f.map{
        case FunDef(fname, params, fbody) =>
          fname -> CloV(params, fbody, Map())  
      }.toMap
      val newEnv = env ++ Closures
      
      Closures.values.foreach(clo => clo.env = newEnv)  
      
      helper(b, newEnv)


    case App(f, args) => helper(f, env) match{
      case CloV(params, ec, fenv) => {
        val avals = args.map(helper(_,env))
        if(args.length != params.length){
          error(s"Wrong arity") 
        }
        helper(ec, fenv ++ (params zip avals))
      }
      case _ => error(s"Call a correct function")
    }
    
    
    case Test(e, t) => helper(e,env) match{
      case BooleanV(_) => 
        if(t==BooleanT) BooleanV(true)
        else BooleanV(false)
      case IntV(_) =>
        if(t==IntT) BooleanV(true)
        else BooleanV(false) 
      case TupleV(_) =>
        if(t==TupleT) BooleanV(true)
        else BooleanV(false) 
      case NilV =>
        if(t==ListT) BooleanV(true)
        else BooleanV(false) 
      case ConsV(_,_) =>
        if(t==ListT) BooleanV(true)
        else BooleanV(false)
      case CloV(_,_,_) =>
        if(t==FunctionT) BooleanV(true)
        else BooleanV(false)
      case _ =>
        error(s"Not Existing Type")
    }
    
  } 
}





