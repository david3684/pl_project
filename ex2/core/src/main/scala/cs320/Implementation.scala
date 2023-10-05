package cs320

object Implementation extends Template {

  def freeIds(expr: Expr): Set[String] = {
    def frees(e: Expr, ids: Set[String]): Set[string] = e match {
      case Num(n) => Set()
      case Add(left, right) => frees(left, ids) ++ frees(right, ids)
      case Sum(left, right) => frees(left, ids) ++ frees(right, ids)
      case Val(name, expr, body) => frees(expr, ids) ++ frees(body, ids+name)
      case Id(id) => if (ids contains id) Set() else Set(id)
    }
    frees(expr, Set())
  }

  def bindingIds(expr: Expr): Set[String] = expr match {
    case Num(n) => Set(n)
    case Add(left, right) => bindingIds(left) ++ bindingIds(right)
    case Sub(left, right) => bindingIds(left) ++ bindingIds(right)
    case Val(name, expr, body) => bindingIds(expr) ++ bindingIds(body) + name
  }

  def boundIds(expr: Expr): Set[String] = ???
}
