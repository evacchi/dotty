import scala.quoted.Staging
class Foo {
  def foo: Unit = {
    def expr(implicit st: Staging) = '{
      val a = 3
      println("foo")
      2 + a
    }
  }
}
