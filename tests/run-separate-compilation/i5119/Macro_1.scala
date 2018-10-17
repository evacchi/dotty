import scala.quoted._
import scala.tasty.Reflection

object Macro {
  class StringContextOps(sc: => StringContext) {
    inline def ff(args: => Any*): String = ~Macro.impl('(sc), '(args))
  }
  implicit inline def XmlQuote(sc: => StringContext): StringContextOps = new StringContextOps(sc)
  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(implicit staging: Staging): Expr[String] = {
    import staging.reflection._
    (sc.reflect.underlyingArgument.show + "\n" + args.reflect.underlyingArgument.show).toExpr
  }
}
