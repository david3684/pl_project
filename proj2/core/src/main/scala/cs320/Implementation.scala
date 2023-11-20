package cs320

import Value._

object Implementation extends Template {


  type ExceptionHandler = Option[ExceptionHandlerFrame]
  case class ExceptionHandlerFrame(expr: Expr, env: Map[String, Value], cont: Value => Value, prevHandler: ExceptionHandler)
  def interp(expr: Expr): Value = helper(expr, Map(), None, x => x)

  def helper(expr: Expr, env: Map[String, Value], h: ExceptionHandler, k: Value => Value ): Value = expr match{
    case NilE => k(NilV)
    case Id(x) => k(env.getOrElse(x, error(s"Free Identifier $x")))
    case IntE(n) => k(IntV(n))
    case BooleanE(b) => k(BooleanV(b))
    case Add(left, right) => helper(left, env, h, lv => {
      helper(right, env, h, rv => {
        (lv, rv) match {
          case (IntV(l), IntV(r)) => k(IntV(l+r))
          case _ => error(s"Not Integer")
        }
      })
    })
    case Mul(left, right) => helper(left, env, h, lv => {
      helper(right, env, h, rv => {
        (lv, rv) match {
          case (IntV(l), IntV(r)) => k(IntV(l*r))
          case _ => error(s"Not Integer")
        }
      })
    })
    case Div(left, right) => helper(left, env, h, lv => {
      helper(right, env, h, rv => {
        (lv, rv) match {
          case (IntV(l), IntV(r)) => if(r!=0) k(IntV(l/r)) else error(s"Zero Division")
          case _ => error(s"Not Integer")
        }
      })
    })
    case Mod(left, right) => helper(left, env, h, lv => {
      helper(right, env, h, rv => {
        (lv, rv) match {
          case (IntV(l), IntV(r)) => if(r!=0) k(IntV(l%r)) else error(s"Zero Division")
          case _ => error(s"Not Integer")
        }
      })
    })
    case Eq(left, right) => helper(left, env, h, lv => {
      helper(right, env, h, rv => {
        (lv, rv) match {
          case (IntV(l), IntV(r)) => k(BooleanV(l==r))
          case _ => error(s"Not Integer")
        }
      })
    })
    case Lt(left, right) => helper(left, env, h, lv => {
      helper(right, env, h, rv => {
        (lv, rv) match {
          case (IntV(l), IntV(r)) => k(BooleanV(l<r))
          case _ => error(s"Not Integer")
        }
      })
    })

    case If(condition, trueBranch, falseBranch) => 
      helper(condition, env, h, cv => cv match {
        case BooleanV(true) => helper(trueBranch, env, h, tv => {
          k(tv)
        })
        case BooleanV(false) => helper(falseBranch, env, h, tv => {
          k(tv)
        })
        case _ => error(s"Not Boolean")
      })

    case TupleE(e) => 
      def evalAll(es: List[Expr], acc: List[Value]): Value = es match {
        case head :: tail => 
          // Evaluate the head, then use its value to recursively evaluate the tail
          helper(head, env, h, headVal => evalAll(tail, headVal :: acc))
        case Nil => 
          // When all expressions are evaluated, apply the continuation to the tuple value
          // Note: The list of accumulated values needs to be reversed as the construction was from right to left
          k(TupleV(acc.reverse))
      }
      // Start evaluating the tuple elements, with an empty accumulator for the values
      evalAll(e, Nil)

    case Proj(e, i) => helper(e, env, h, ev =>{
      ev match{
        case TupleV(values) =>
          if(i>=1 && i <=values.length) k(values(i-1))
          else error(s"Index out of bound")
        case _ => error(s"Projection Can be applied to Tuple")
      }
    })
    case ConsE(head, tail) => helper(head, env, h, hv => {
      helper(tail, env, h, tv => {
        (hv, tv) match{
          case (_, NilV) => k(ConsV(hv, NilV))
          case (_, ConsV(_,_)) => k(ConsV(hv, tv))
          case _ => error(s"Second Argument should be a list")
        }
      })
    })
    case Empty(l) =>
      helper(l, env, h, listVal => {
        listVal match {
          case NilV => k(BooleanV(true))
          case ConsV(_, _) => k(BooleanV(false))
          case _ => error(s"Given expr is not a list")
        }
      })
    case Head(l) => 
    helper(l, env, h, listVal => {
      listVal match {
        case ConsV(head, _) => k(head)
        case _ => error(s"Given expr is not a list")
      }
    })
    case Tail(l) => 
      helper(l, env, h, listVal => {
        listVal match {
          case ConsV(_, tail) => k(tail)
          case _ => error(s"Given expr is not a list")
        }
      })
    case Val(x,e,b) => 
      helper(e, env, h, ev => {
        helper(b, env+(x->ev), h, k)
      })
    case Vcc(x,e) =>
      val newEnv = env + (x->ContV(k))
      helper(e, newEnv, h, k)
    case Fun(params, b) => k(CloV(params, b, env))
    
    case RecFuns(f, b) => {
      val cloV = f.map({fun=> CloV(fun.parameters, fun.body , env)})
      val names = f.map({fun=>fun.name})
      for(c <- cloV) c.env = env ++ names.zip(cloV).toMap
      helper(b, env ++ names.zip(cloV).toMap, h, k)
    }
    case App(f, args) => {
      helper(f, env, h, fv => 
        helper(TupleE(args), env, h, av => {
          fv match{
            case CloV(params, b, fenv) => {
              av match {
                case TupleV(avals) => {
                  if(avals.length != params.length){
                    error(s"Wrong arity") 
                  }
                  else{
                    helper(b, fenv ++ (params zip avals), h, k)
                  }
                }
                case _ => error(s"Error")
              }
            }
            case ContV(c) => {
              av match {
                case TupleV(values) => {
                  if(values.length == 1){
                    c(values(0))
                  }
                  else {
                    error(s"ERROR")
                  }
                }
              }
            }
            case _ => error(s"Neither Closure or Continuation")
          }
        })
      )
    }
    case Throw(e) => {
      helper(e, env, h, ev => {
        h match {
          case Some(ExceptionHandlerFrame(eh, handlerEnv, cont, prevHandler)) =>
            helper(eh, handlerEnv, prevHandler, ehv => ehv match{
              case CloV(param, body, closureEnv) if param.length ==1 =>
                val newEnv = closureEnv + (param.head -> ev)
                helper(body, newEnv, prevHandler, bv => {
                  cont(bv)
                })
              case ContV(c) => {
                c(ev);
              }
              case _ => 
                error(s"Exception handler must be a closure or a continuation")
            })
          case None => 
            error(s"No exception handler registered")
        }
      })
    }

    case Try(e1, e2) => {
      val newHandler = Some(ExceptionHandlerFrame(e2, env, k, h))
      helper(e1, env, newHandler, ev => {
        k(ev)
      })
    }
    case Test(e, t) => 
      helper(e,env, h, ev => ev match{
        case BooleanV(_) => 
          if(t==BooleanT) k(BooleanV(true))
          else k(BooleanV(false))
        case IntV(_) =>
          if(t==IntT) k(BooleanV(true))
          else k(BooleanV(false))
        case TupleV(_) =>
          if(t==TupleT) k(BooleanV(true))
          else k(BooleanV(false))
        case NilV =>
          if(t==ListT) k(BooleanV(true))
          else k(BooleanV(false))
        case ConsV(_,_) =>
          if(t==ListT) k(BooleanV(true))
          else k(BooleanV(false))
        case CloV(_,_,_) =>
          if(t==FunctionT) k(BooleanV(true))
          else k(BooleanV(false))
        case ContV(_) =>
          if(t==FunctionT) k(BooleanV(true))
          else k(BooleanV(false))
        case _ =>
          error(s"Not Existing Type")
      })
    }
  }


