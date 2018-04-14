package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import scala.tasty.{trees, types}

object ValDef {

  def apply(tree: tpd.ValDef)(implicit ctx: Context): trees.ValDef = Impl(tree, ctx)

  def unapplyValDef(tree: scala.tasty.Tree): Option[trees.ValDef.Data] = tree match {
    case Impl(vdef, ctx) =>
      implicit val ctx_ = ctx
      Some((TermName(vdef.name), TypeTree(vdef.tpt), if (vdef.rhs.isEmpty) None else Some(Term(vdef.rhs)), Modifiers(vdef)))
    case _ => None
  }

  private case class Impl(tree: tpd.ValDef, ctx: Context) extends trees.ValDef with Positioned {

    def tpe: types.Type = Type(tree.tpe)(ctx)

    def sym: scala.tasty.Symbol = TastySymbol(tree.symbol(ctx))(ctx)

    override def toString: String = {
      import Toolbox.extractor
      val trees.ValDef(name, tpt, rhs, mods) = this
      s"ValDef($name, $tpt, $rhs, $mods)"
    }

  }

}
