import scala.quoted._
import scala.tasty.Reflection

object SourceFiles {

  implicit inline def getThisFile: String =
    ~getThisFileImpl

  private def getThisFileImpl(implicit staging: Staging): Expr[String] = {
    import staging.reflection._
    rootContext.source.getFileName.toString.toExpr
  }

}
