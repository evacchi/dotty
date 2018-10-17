import scala.quoted._

object Test {

  def a[A: Type]()(implicit st: Staging): Unit = {
    b[Expr[A]]()
    a[A]()
  }

  def b[A: Type](): Unit = ???
}
