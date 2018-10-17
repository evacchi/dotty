import scala.quoted._

import scala.tasty._

object Macros {

  implicit inline def printType[T]: Unit = ~impl('[T])

  def impl[T](x: Type[T])(implicit staging: Staging): Expr[Unit] = {
    import staging.reflection._

    val tree = x.reflect
    '{
      println(~tree.show.toExpr)
      println(~tree.tpe.show.toExpr)
      println()
    }
  }
}
