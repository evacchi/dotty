import scala.quoted._

import scala.tasty._

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line: LineNumber = ~lineImpl

  def lineImpl(implicit staging: Staging): Expr[LineNumber] = {
    import staging.reflection._
    '(new LineNumber(~rootPosition.startLine.toExpr))
  }

}
