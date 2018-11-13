package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core._
import dotty.tools.dotc.tastyreflect.FromSymbol.{definitionFromSym, packageDefFromSym}

trait TreeOpsImpl extends scala.tasty.reflect.TreeOps with CoreImpl with Helpers {

  def TreeDeco(tree: Tree): TreeAPI = new TreeAPI {
    def pos(implicit ctx: Context): Position = tree.pos
    def symbol(implicit ctx: Context): Symbol = tree.symbol
  }

  object IsPackageClause extends IsPackageClauseModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageClause] = tree match {
      case x: tpd.PackageDef => Some(x)
      case _ => None
    }
  }

  object PackageClause extends PackageClauseModule {
    def apply(pid: Ref, stats: List[Tree])(implicit ctx: Context): PackageClause =
      tpd.PackageDef(pid.asInstanceOf[tpd.RefTree], stats)

    def unapply(tree: Tree)(implicit ctx: Context): Option[(Ref, List[Tree])] = tree match {
      case x: tpd.PackageDef => Some((x.pid, x.stats))
      case _ => None
    }
  }

  def PackageClauseDeco(pack: PackageClause): PackageClauseAPI = new PackageClauseAPI {

  }

  // ----- Statements -----------------------------------------------

  object Import extends ImportModule {
    def apply(expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import =
      tpd.Import(expr, selectors)

    def unapply(x: Tree)(implicit ctx: Context): Option[(Term, List[ImportSelector])] = x match {
      case x: tpd.Import => Some((x.expr, x.selectors))
      case _ => None
    }
  }

  def ImportDeco(imp: Import): ImportAPI = new ImportAPI {
    def expr(implicit ctx: Context): Tree = imp.expr
    def selector(implicit ctx: Context): List[ImportSelector] = imp.selectors
  }

  // ----- Definitions ----------------------------------------------

  object IsDefinition extends IsDefinitionModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Definition] = tree match {
      case tree: tpd.MemberDef => Some(tree)
      case tree: PackageDefinition => Some(tree)
      case _ => None
    }
  }

  def DefinitionDeco(definition: Definition): DefinitionAPI = new DefinitionAPI {
    def name(implicit ctx: Context): String = definition.symbol.name.toString
  }

  // ClassDef

  object IsClassDef extends IsClassDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ClassDef] = tree match {
      case x: tpd.TypeDef if x.isClassDef => Some(x)
      case _ => None
    }
  }

  object ClassDef extends ClassDefModule {
    def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef = {
      ???
    }

    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[TermOrTypeTree],  Option[ValDef], List[Statement])] = tree match {
      case Trees.TypeDef(name, impl: tpd.Template) =>
        Some((name.toString, impl.constr, impl.parents, optional(impl.self), impl.body))
      case _ => None
    }
  }

  def ClassDefDeco(cdef: ClassDef): ClassDefAPI = new ClassDefAPI {
    private def rhs = cdef.rhs.asInstanceOf[tpd.Template]
    def constructor(implicit ctx: Context): DefDef = rhs.constr
    def parents(implicit ctx: Context): List[tpd.Tree] = rhs.parents
    def self(implicit ctx: Context): Option[tpd.ValDef] = optional(rhs.self)
    def body(implicit ctx: Context): List[tpd.Tree] = rhs.body
    def symbol(implicit ctx: Context): ClassSymbol = cdef.symbol.asClass
  }

  // DefDef

  object IsDefDef extends IsDefDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[DefDef] = tree match {
      case x: tpd.DefDef => Some(x)
      case _ => None
    }
  }

  object DefDef extends DefDefModule {
    def apply(symbol: DefSymbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(implicit ctx: Context): DefDef =
      tpd.polyDefDef(symbol, tparams => vparamss => rhsFn(tparams)(vparamss).getOrElse(tpd.EmptyTree))

    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])] = tree match {
      case x: tpd.DefDef =>
        Some((x.name.toString, x.tparams, x.vparamss, x.tpt, optional(x.rhs)))
      case _ => None
    }
  }

  def DefDefDeco(ddef: DefDef): DefDefAPI = new DefDefAPI {
    def typeParams(implicit ctx: Context): List[TypeDef] = ddef.tparams
    def paramss(implicit ctx: Context): List[List[ValDef]] = ddef.vparamss
    def returnTpt(implicit ctx: Context): TypeTree = ddef.tpt
    def rhs(implicit ctx: Context): Option[Tree] = optional(ddef.rhs)
    def symbol(implicit ctx: Context): DefSymbol = ddef.symbol.asTerm
  }

  // ValDef

  object IsValDef extends IsValDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ValDef] = tree match {
      case x: tpd.ValDef => Some(x)
      case _ => None
    }
  }

  object ValDef extends ValDefModule {
    def apply(symbol: ValSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef =
      tpd.ValDef(symbol, rhs.getOrElse(tpd.EmptyTree))

    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])] = tree match {
      case x: tpd.ValDef =>
        Some((x.name.toString, x.tpt, optional(x.rhs)))
      case _ => None
    }
  }

  def ValDefDeco(vdef: ValDef): ValDefAPI = new ValDefAPI {
    def tpt(implicit ctx: Context): TypeTree = vdef.tpt
    def rhs(implicit ctx: Context): Option[Tree] = optional(vdef.rhs)
    def symbol(implicit ctx: Context): ValSymbol = vdef.symbol.asTerm
  }

  // TypeDef

  object IsTypeDef extends IsTypeDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeDef] = tree match {
      case x: tpd.TypeDef if !x.symbol.isClass => Some(x)
      case _ => None
    }
  }

  object TypeDef extends TypeDefModule {
    def apply(symbol: TypeSymbol)(implicit ctx: Context): TypeDef = tpd.TypeDef(symbol)

    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)] = tree match {
      case x: tpd.TypeDef if !x.symbol.isClass => Some((x.name.toString, x.rhs))
      case _ => None
    }
  }

  def TypeDefDeco(tdef: TypeDef): TypeDefAPI = new TypeDefAPI {
    def rhs(implicit ctx: Context): TypeOrBoundsTree = tdef.rhs
    def symbol(implicit ctx: Context): TypeSymbol = tdef.symbol.asType
  }

  // PackageDef

  def PackageDefDeco(pdef: PackageDef): PackageDefAPI = new PackageDefAPI {

    def owner(implicit ctx: Context): PackageDefinition = packageDefFromSym(pdef.symbol.owner)

    def members(implicit ctx: Context): List[Statement] = {
      if (pdef.symbol.is(core.Flags.JavaDefined)) Nil // FIXME should also support java packages
      else pdef.symbol.info.decls.iterator.map(definitionFromSym).toList
    }

    def symbol(implicit ctx: Context): PackageSymbol = pdef.symbol
  }

  object IsPackageDef extends IsPackageDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageDef] = tree match {
      case x: PackageDefinition => Some(x)
      case _ => None
    }
  }

  object PackageDef extends PackageDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, PackageDef)] = tree match {
      case x: PackageDefinition =>
        Some((x.symbol.name.toString, packageDefFromSym(x.symbol.owner)))
      case _ => None
    }
  }

  // ----- Terms ----------------------------------------------------

  def TermDeco(term: Term): TermAPI = new TermAPI {
    import tpd._
    def pos(implicit ctx: Context): Position = term.pos
    def tpe(implicit ctx: Context): Type = term.tpe
    def underlyingArgument(implicit ctx: Context): Term = term.underlyingArgument
    def underlying(implicit ctx: Context): Term = term.underlying
  }

  object IsTerm extends IsTermModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term] =
      if (tree.isTerm) Some(tree) else None
    def unapply(termOrTypeTree: TermOrTypeTree)(implicit ctx: Context, dummy: DummyImplicit): Option[Term] =
      if (termOrTypeTree.isTerm) Some(termOrTypeTree) else None
  }

  object Term extends TermModule {

    object Ref extends RefModule {
      def apply(sym: Symbol)(implicit ctx: Context): Ref = tpd.ref(sym).asInstanceOf[tpd.RefTree]
      def copy(original: Tree)(name: String)(implicit ctx: Context): Ref = ???
    }

    object Ident extends IdentModule {
      def apply(name: String)(implicit ctx: Context): Ident = ???

      def copy(original: Tree)(name: String)(implicit ctx: Context): Ident = ???

      def unapply(x: Term)(implicit ctx: Context): Option[String] = x match {
        case x: tpd.Ident if x.isTerm => Some(x.name.show)
        case _ => None
      }
    }

    object Select extends SelectModule {
      def apply(qualifier: Term, name: String, signature: Option[Signature])(implicit ctx: Context): Select = ???

      def copy(original: Tree)(qualifier: Term, name: String)(implicit ctx: Context): Select = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, String, Option[Signature])] = x match {
        case x: tpd.Select if x.isTerm =>
          val sig =
            if (x.symbol.signature == core.Signature.NotAMethod) None
            else Some(x.symbol.signature)
          Some((x.qualifier, x.name.toString, sig))
        case _ => None
      }
    }

    object Literal extends LiteralModule {
      def apply(constant: Constant)(implicit ctx: Context): Literal =
        tpd.Literal(constant)

      def copy(original: Tree)(constant: Constant)(implicit ctx: Context): Literal = ???

      def unapply(x: Term)(implicit ctx: Context): Option[Constant] = x match {
        case Trees.Literal(const) => Some(const)
        case _ => None
      }
    }

    object This extends ThisModule {
      def apply(cls: ClassSymbol)(implicit ctx: Context): This = tpd.This(cls)

      def copy(original: Tree)(qual: Option[Id]): This = ???

      def unapply(x: Term)(implicit ctx: Context): Option[Option[Id]] = x match {
        case Trees.This(qual) => Some(optional(qual))
        case _ => None
      }
    }

    object New extends NewModule {
      def apply(tpt: TypeTree)(implicit ctx: Context): New = tpd.New(tpt)

      def copy(original: Tree)(tpt: TypeTree)(implicit ctx: Context): New = ???

      def unapply(x: Term)(implicit ctx: Context): Option[TypeTree] = x match {
        case x: tpd.New => Some(x.tpt)
        case _ => None
      }
    }

    object NamedArg extends NamedArgModule {
      def apply(name: String, arg: Term)(implicit ctx: Context): NamedArg =
        tpd.NamedArg(name.toTermName, arg)

      def copy(tree: NamedArg)(name: String, arg: Term)(implicit ctx: Context): NamedArg = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(String, Term)] = x match {
        case x: tpd.NamedArg if x.name.isInstanceOf[Names.TermName] => Some((x.name.toString, x.arg))
        case _ => None
      }
    }

    object Apply extends ApplyModule {
      def apply(fn: Term, args: List[Term])(implicit ctx: Context): Apply =
        tpd.Apply(fn, args)

      def copy(original: Tree)(fun: Term, args: List[Term])(implicit ctx: Context): Apply = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[Term])] = x match {
        case x: tpd.Apply => Some((x.fun, x.args))
        case _ => None
      }
    }

    object TypeApply extends TypeApplyModule {
      def apply(fn: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply =
        tpd.TypeApply(fn, args)

      def copy(original: Tree)(fun: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[TypeTree])] = x match {
        case x: tpd.TypeApply => Some((x.fun, x.args))
        case _ => None
      }
    }

    object Super extends SuperModule {
      def apply(qual: Term, mix: Option[Id])(implicit ctx: Context): Super =
        tpd.Super(qual, mix.getOrElse(untpd.EmptyTypeIdent), ???, ???)

      def copy(original: Tree)(qual: Term, mix: Option[Id]): Super = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Option[Id])] = x match {
        case x: tpd.Super => Some((x.qual, if (x.mix.isEmpty) None else Some(x.mix)))
        case _ => None
      }
    }

    object Typed extends TypedModule {
      def apply(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed =
        tpd.Typed(expr, tpt)

      def copy(original: Tree)(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, TypeTree)] = x match {
        case x: tpd.Typed => Some((x.expr, x.tpt))
        case _ => None
      }
    }

    object Assign extends AssignModule {
      def apply(lhs: Term, rhs: Term)(implicit ctx: Context): Assign =
        tpd.Assign(lhs, rhs)

      def copy(original: Tree)(lhs: Term, rhs: Term)(implicit ctx: Context): Assign = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)] = x match {
        case x: tpd.Assign => Some((x.lhs, x.rhs))
        case _ => None
      }
    }

    object Block extends BlockModule {
      def apply(stats: List[Statement], expr: Term)(implicit ctx: Context): Block =
        tpd.Block(stats, expr)

      def copy(original: Tree)(stats: List[Statement], expr: Term)(implicit ctx: Context): Block = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(List[Statement], Term)] = normalizedLoops(x) match {
        case Trees.Block(stats, expr) => Some((stats, expr))
        case _ => None
      }
      /** Normalizes non Blocks.
       *  i) Put `while` and `doWhile` loops in their own blocks: `{ def while$() = ...; while$() }`
       *  ii) Put closures in their own blocks: `{ def anon$() = ...; closure(anon$, ...) }`
       */
      private def normalizedLoops(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
        case block: tpd.Block if block.stats.size > 1 =>
          def normalizeInnerLoops(stats: List[tpd.Tree]): List[tpd.Tree] = stats match {
            case (x: tpd.DefDef) :: y :: xs if needsNormalization(y) =>
              tpd.Block(x :: Nil, y) :: normalizeInnerLoops(xs)
            case x :: xs => x :: normalizeInnerLoops(xs)
            case Nil => Nil
          }
          if (needsNormalization(block.expr)) {
            val stats1 = normalizeInnerLoops(block.stats.init)
            val normalLoop = tpd.Block(block.stats.last :: Nil, block.expr)
            tpd.Block(stats1, normalLoop)
          } else {
            val stats1 = normalizeInnerLoops(block.stats)
            tpd.cpy.Block(block)(stats1, block.expr)
          }
        case _ => tree
      }

      /** If it is the second statement of a closure. See: `normalizedLoops` */
      private def needsNormalization(tree: tpd.Tree)(implicit ctx: Context): Boolean = tree match {
        case _: tpd.Closure => true
        case _ => false
      }
    }

    object Lambda extends LambdaModule {
      def apply(meth: Term, tpt: Option[TypeTree])(implicit ctx: Context): Lambda =
        tpd.Closure(Nil, meth, tpt.getOrElse(tpd.EmptyTree))

      def copy(original: Tree)(meth: Tree, tpt: Tree)(implicit ctx: Context): Lambda = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Option[TypeTree])] = x match {
        case x: tpd.Closure => Some((x.meth, optional(x.tpt)))
        case _ => None
      }
    }

    object If extends IfModule {
      def apply(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If =
        tpd.If(cond, thenp, elsep)

      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term, Term)] = x match {
        case x: tpd.If => Some((x.cond, x.thenp, x.elsep))
        case _ => None
      }
    }

    object Match extends MatchModule {
      def apply(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match =
        tpd.Match(selector, cases)

      def copy(original: Tree)(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[CaseDef])] = x match {
        case x: tpd.Match => Some((x.selector, x.cases))
        case _ => None
      }
    }

    object Try extends TryModule {
      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try =
        tpd.Try(expr, cases, finalizer.getOrElse(tpd.EmptyTree))

      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])] = x match {
        case x: tpd.Try => Some((x.expr, x.cases, optional(x.finalizer)))
        case _ => None
      }
    }

    object Return extends ReturnModule {
      def apply(expr: Term)(implicit ctx: Context): Return =
        tpd.Return(expr, ctx.owner)

      def copy(original: Tree)(expr: Term)(implicit ctx: Context): Return = ???

      def unapply(x: Term)(implicit ctx: Context): Option[Term] = x match {
        case x: tpd.Return => Some(x.expr)
        case _ => None
      }
    }

    object Repeated extends RepeatedModule {
      def apply(elems: List[Term])(implicit ctx: Context): Repeated =
        tpd.SeqLiteral(elems, ???)

      def copy(original: Tree)(elems: List[Term])(implicit ctx: Context): Repeated = ???

      def unapply(x: Term)(implicit ctx: Context): Option[List[Term]] = x match {
        case x: tpd.SeqLiteral => Some(x.elems)
        case _ => None
      }
    }

    object Inlined extends InlinedModule {
      def apply(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined =
        tpd.Inlined(call.getOrElse(tpd.EmptyTree), bindings.map { case b: tpd.MemberDef => b }, expansion)

      def copy(original: Tree)(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Option[TermOrTypeTree], List[Statement], Term)] = x match {
        case x: tpd.Inlined =>
          Some((optional(x.call), x.bindings, x.expansion))
        case _ => None
      }
    }

    object SelectOuter extends SelectOuterModule {
      def apply(qualifier: Term, levels: Int, tpe: Type)(implicit ctx: Context): SelectOuter = ???

      def copy(original: Tree)(qualifier: Term, levels: Int, tpe: Type)(implicit ctx: Context): SelectOuter = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Int, Type)] = x match {
        case x: tpd.Select =>
          x.name match {
            case NameKinds.OuterSelectName(_, levels) => Some((x.qualifier, levels, x.tpe.stripTypeVar))
            case _ => None
          }
        case _ => None
      }
    }

    object While extends WhileModule {
      def apply(cond: Term, body: Term)(implicit ctx: Context): While =
        tpd.WhileDo(cond, body)

      def copy(original: Tree)(cond: Term, body: Term)(implicit ctx: Context): While = ???

      def unapply(x: Term)(implicit ctx: Context): Option[(Term, Term)] = x match {
        case x: tpd.WhileDo => Some((x.cond, x.body))
        case _ => None
      }
    }
  }

  def termAsTermOrTypeTree(term: Term): TermOrTypeTree = term
}
