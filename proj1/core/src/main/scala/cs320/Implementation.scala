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
          else error(s"Can't Divide by Zero")
        case _ => error(s"Both operands should be integer")
      }
    case Div(left, right) => 
      val leftVal = helper(left, env)
      val rightVal = helper(right, env)
      (leftVal, rightVal) match {
        case (IntV(x), IntV(y)) => 
          if(y!=0) IntV(x/y)
          else error(s"Can't Divide by Zero")
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
          else error(s"Index Out of Bound of length ${values.length}")
        case _ => error(s"Projection can be applied for tuples")
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
    case RecFuns(f, b) => 
      // Create a new environment with dummy bindings for the function names
      val dummyEnv = f.foldLeft(env) {
        case(currentEnv, FunDef(fname,params, _)) => 
          currentEnv + (fname -> CloV(params, IntE(0), currentEnv))
      }
      // Create actual closures with the dummy environment
      val funClosures = f.map {
        case FunDef(fname, params, fbody) => 
          fname -> CloV(params, fbody, dummyEnv)
      }.toMap
        // Update the dummy environment with the actual closures
      val newEnv = dummyEnv ++ funClosures
      // Evaluate the body expression in the updated environment
      helper(b, newEnv)
    case App(f, args) => helper(f, env) match{
      case CloV(params, b, fenv) => {
        val avals = args.map(helper(_,env))
        if(args.length != params.length){
          error(s"Function application error: Expected ${params.length} parameters, but got ${args.length}.")
          error(s"Expected parameters: $params")
          error(s"Received arguments: $args")
          error(s"Wrong Arity") 
        } 
        helper(b, fenv ++ (params zip avals))
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

/*RecFuns(
  List(
    FunDef(
      f,
      List(x),
      If(
        Eq(
          Id(x),
          IntE(0)
          ),
        IntE(0),
        Add(Id(x),
        App(
          Id(f),
          List(
            Add(
              Id(x),
              Mul(
                IntE(1),
                IntE(-1)
                )
              )
            )
          )
        )
      )
    )
  ),
  App(
    Id(f),
    List(
      IntE(3)
      )
    )
  )*/