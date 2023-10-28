package cs320

import Value._

object Implementation extends Template {
  
  type ExceptionHandler = (Expr, Map[String, Value], Value => Value)
  def interp(expr: Expr): Value = helper(expr, Map(), Nil, x => x)

  def helper(expr: Expr, env:Map[String, Value], h: List[ExceptionHandler], k: Value => Value ): Value = expr match{
    case NilE => k(NilV)
    case Id(x) => k(env.getOrElse(x, error(s"Free Identifier $x")))
    case IntE(n) => k(IntV(n))
    case BooleanE(b) => k(BooleanV(b))
    case Add(left, right) => 
      helper(left, env, h, lv => {
        helper(right, env, h, rv => {
          (lv, rv) match {
            case (IntV(l), IntV(r)) => k(IntV(l + r))
            case _ => error("Both arguments to Add must be integers")
          }
        })
      })
    case Mul(left, right) => 
      helper(left, env, h, lv => {
        helper(right, env, h, rv => {
          (lv, rv) match {
            case (IntV(l), IntV(r)) => k(IntV(l * r))
            case _ => error("Both arguments to Mul must be integers")
          }
        })
      })
    case Eq(left, right) => 
      helper(left, env, h, lv => {
        helper(right, env, h, rv => {
          k(BooleanV(lv == rv))
        })
      })
    case Lt(left, right) => 
      helper(left, env, h, lv => {
        helper(right, env, h, rv => {
          (lv, rv) match {
            case (IntV(l), IntV(r)) => k(BooleanV(l < r))
            case _ => error("Both arguments to Lt must be integers")
          }
        })
      })
    
    case Mod(left, right) => 
      helper(left, env, h, lv => {
        helper(right, env, h, rv => {
          (lv, rv) match{
            case (IntV(l), IntV(r)) => k(IntV(l % r))
            case _ => error("Both arguments to Mod must be integers")
          }
        })
      })

    case Div(left, right) => 
      helper(left, env, h, lv => {
        helper(right, env, h, rv => {
          (lv, rv) match{
            case (IntV(l), IntV(r)) => if(r==0) error(s"Can't Devide By Zero")
            else k(IntV(l/r))
            case _ => error("Both arguments to Div must be integers")
          }
        })
      })
    case If(condition, trueBranch, falseBranch) => {
      helper(condition, env, h, cv => {
        cv match{
          case BooleanV(true) => 
            helper(trueBranch, env, h, tv => {
              k(tv)
            }) 
          case BooleanV(false) => 
            helper(falseBranch, env, h, fv => {
              k(fv)
            })
        }
      })
    }
    case TupleE(e) => 
        def evalTuple(es: List[Expr], acc: List[Value]): Value = es match {
        case Nil => k(TupleV(acc.reverse))
        case e :: rest => helper(e, env, h, v => evalTuple(rest, v :: acc))
      }
      evalTuple(e, Nil)
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
        helper(b,env+(x->ev), h, bv=>{
          k(bv)
        })
      })
    case Vcc(x,e) =>
      val newEnv = env + (x->ContV(k))
      helper(e, newEnv, h, k)
    case Fun(params, b) => k(CloV(params, b, env))
    case App(f, args) => 
      
    }
  }
  

