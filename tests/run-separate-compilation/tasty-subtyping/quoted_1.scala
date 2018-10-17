import scala.quoted._

import scala.tasty._

object Macros {

  inline def isTypeEqual[T, U]: Boolean =
    ~isTypeEqualImpl('[T], '[U])

  inline def isSubTypeOf[T, U]: Boolean =
    ~isSubTypeOfImpl('[T], '[U])

  def isTypeEqualImpl[T, U](t: Type[T], u: Type[U])(implicit staging: Staging): Expr[Boolean] = {
    import staging.reflection._
    val isTypeEqual = t.reflect.tpe =:= u.reflect.tpe
    isTypeEqual.toExpr
  }

  def isSubTypeOfImpl[T, U](t: Type[T], u: Type[U])(implicit staging: Staging): Expr[Boolean] = {
    import staging.reflection._
    val isTypeEqual = t.reflect.tpe <:< u.reflect.tpe
    isTypeEqual.toExpr
  }
}
