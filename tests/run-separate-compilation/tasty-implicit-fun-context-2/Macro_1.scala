import scala.quoted._
import scala.tasty.Reflection

object Foo {

  type Macro[X] = implicit Staging => Expr[X]
  type Tastier[X] = implicit Staging => X

  implicit inline def foo: String =
    ~fooImpl

  def fooImpl(implicit staging: Staging): implicit Staging => Tastier[implicit Staging => Macro[String]] = {
    '("abc")
  }

}
