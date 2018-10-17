import scala.quoted._
import scala.tasty.Reflection

object SourceFiles {

  type Macro[X] = implicit Staging => Expr[X]

  implicit inline def getThisFile: String =
    ~getThisFileImpl

  def getThisFileImpl: Macro[String] = {
    val staging = implicitly[Staging]
    import staging.reflection._
    rootContext.source.getFileName.toString.toExpr
  }


}
