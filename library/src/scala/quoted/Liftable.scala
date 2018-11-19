package scala.quoted

import scala.runtime.quoted.Unpickler.liftedExpr

/** A typeclass for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'(...)` operation.
 */
abstract class Liftable[T] {
  def toExpr(x: T)(implicit st: Staging): Expr[T]
}

/** Some liftable base types. To be completed with at least all types
 *  that are valid Scala literals. The actual implementation of these
 *  typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
 *  gives an alternative implementation using just the basic staging system.
 */
object Liftable {

  implicit def BooleanIsLiftable: Liftable[Boolean] = new Liftable[Boolean] {
    def toExpr(x: Boolean)(implicit st: Staging): Expr[Boolean] = liftedExpr(x)
  }

  implicit def ByteIsLiftable: Liftable[Byte] = new Liftable[Byte] {
    def toExpr(x: Byte)(implicit st: Staging): Expr[Byte] = liftedExpr(x)
  }

  implicit def CharIsLiftable: Liftable[Char] = new Liftable[Char] {
    def toExpr(x: Char)(implicit st: Staging): Expr[Char] = liftedExpr(x)
  }

  implicit def ShortIsLiftable: Liftable[Short] = new Liftable[Short] {
    def toExpr(x: Short)(implicit st: Staging): Expr[Short] = liftedExpr(x)
  }

  implicit def IntIsLiftable: Liftable[Int] = new Liftable[Int] {
    def toExpr(x: Int)(implicit st: Staging): Expr[Int] = liftedExpr(x)
  }

  implicit def LongIsLiftable: Liftable[Long] = new Liftable[Long] {
    def toExpr(x: Long)(implicit st: Staging): Expr[Long] = liftedExpr(x)
  }

  implicit def FloatIsLiftable: Liftable[Float] = new Liftable[Float] {
    def toExpr(x: Float)(implicit st: Staging): Expr[Float] = liftedExpr(x)
  }

  implicit def DoubleIsLiftable: Liftable[Double] = new Liftable[Double] {
    def toExpr(x: Double)(implicit st: Staging): Expr[Double] = liftedExpr(x)
  }

  implicit def StringIsLiftable: Liftable[String] = new Liftable[String] {
    def toExpr(x: String)(implicit st: Staging): Expr[String] = liftedExpr(x)
  }

  implicit def ClassIsLiftable[T]: Liftable[Class[T]] = new Liftable[Class[T]] {
    def toExpr(x: Class[T])(implicit st: Staging): Expr[Class[T]] = liftedExpr(x)
  }

}
