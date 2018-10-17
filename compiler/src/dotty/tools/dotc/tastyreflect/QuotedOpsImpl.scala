package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Contexts.FreshContext
import dotty.tools.dotc.core.quoted.{PickledQuotes, Quoted}
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.diagnostic.MessageContainer

trait QuotedOpsImpl extends scala.tasty.reflect.QuotedOps with CoreImpl {

  def QuotedExprDeco[T](x: scala.quoted.Expr[T]): QuotedExprAPI = new QuotedExprAPI {
    def unseal(implicit ctx: Context): Term = PickledQuotes.quotedExprToTree(x)
  }

  def QuotedTypeDeco[T](x: scala.quoted.Type[T]): QuotedTypeAPI = new QuotedTypeAPI {
    def unseal(implicit ctx: Context): TypeTree = PickledQuotes.quotedTypeToTree(x)
  }

  def TermToQuoteDeco(term: Term): TermToQuotedAPI = new TermToQuotedAPI {

    def seal[T: scala.quoted.Type](implicit ctx: Context): scala.quoted.Expr[T] = {
      typecheck(ctx)
      new scala.quoted.Exprs.TastyTreeExpr(term, PickledQuotes.contextId).asInstanceOf[scala.quoted.Expr[T]]
    }

    private def typecheck[T: scala.quoted.Type](ctx: Context): Unit = {
      implicit val ctx0: FreshContext = ctx.fresh
      ctx0.setTyperState(ctx0.typerState.fresh())
      ctx0.typerState.setReporter(new Reporter {
        def doReport(m: MessageContainer)(implicit ctx: Context): Unit = ()
      })
      val tp = QuotedTypeDeco(implicitly[scala.quoted.Type[T]]).unseal
      ctx0.typer.typed(term, tp.tpe)
      if (ctx0.reporter.hasErrors) {
        val stack = new Exception().getStackTrace
        def filter(elem: StackTraceElement) =
          elem.getClassName.startsWith("dotty.tools.dotc.tasty.ReflectionImpl") ||
            !elem.getClassName.startsWith("dotty.tools.dotc")
        throw new scala.tasty.TastyTypecheckError(
          s"""Error during tasty reflection while typing term
             |term: ${term.show}
             |with expected type: ${tp.tpe.show}
             |
               |  ${stack.takeWhile(filter).mkString("\n  ")}
             """.stripMargin
        )
      }
    }
  }
}
