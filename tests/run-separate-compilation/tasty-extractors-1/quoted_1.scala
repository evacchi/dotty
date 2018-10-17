import scala.quoted._

import scala.tasty._

object Macros {

  implicit inline def printTree[T](x: => T): Unit =
    ~impl('(x))

  def impl[T](x: Expr[T])(implicit staging: Staging): Expr[Unit] = {
    import staging.reflection._

    val tree = x.reflect
    val treeStr = tree.show
    val treeTpeStr = tree.tpe.show

    '{
      println(~treeStr.toExpr)
      println(~treeTpeStr.toExpr)
      println()
    }
  }
}
