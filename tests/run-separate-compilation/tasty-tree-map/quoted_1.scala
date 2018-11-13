import scala.quoted._

import scala.tasty._


object Macros {

  implicit inline def identityMaped(x: => Unit): Unit = ~impl('(x))

  def impl(x: Expr[Unit])(implicit staging: Staging): Expr[Unit] = {
    import staging.reflection._
    val identityMap = new TreeMap { }
    val transformed = identityMap.transformTerm(x.reflect).reify[Unit]
    '(println(~transformed))
  }

}
