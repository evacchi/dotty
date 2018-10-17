import scala.quoted._
import scala.tasty.Reflection

object Macro {

  inline def ff(arg1: Any,  arg2: Any): String = ~Macro.impl('(arg1), '(arg2))

  def impl(arg1: Expr[Any], arg2: Expr[Any])(implicit staging: Staging): Expr[String] = {
    import staging.reflection._
    (arg1.reflect.underlyingArgument.show + "\n" + arg2.reflect.underlyingArgument.show).toExpr
  }

}
