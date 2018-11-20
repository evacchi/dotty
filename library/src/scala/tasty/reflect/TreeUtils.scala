package scala.tasty
package reflect

/** Tasty reflect case definition */
trait TreeUtils
    extends Core
    with CaseDefOps
    with PatternOps
    with SymbolOps
    with TreeOps
    with TypeOrBoundsTreeOps {

  abstract class TreeAccumulator[X] {

    // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
    def foldTree(x: X, tree: Tree)(implicit ctx: Context): X
    def foldTypeTree(x: X, tree: TypeOrBoundsTree)(implicit ctx: Context): X
    def foldCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X
    def foldPattern(x: X, tree: Pattern)(implicit ctx: Context): X

    def foldTrees(x: X, trees: Iterable[Tree])(implicit ctx: Context): X = (x /: trees)(foldTree)
    def foldTypeTrees(x: X, trees: Iterable[TypeOrBoundsTree])(implicit ctx: Context): X = (x /: trees)(foldTypeTree)
    def foldCaseDefs(x: X, trees: Iterable[CaseDef])(implicit ctx: Context): X = (x /: trees)(foldCaseDef)
    def foldPatterns(x: X, trees: Iterable[Pattern])(implicit ctx: Context): X = (x /: trees)(foldPattern)
    private def foldParents(x: X, trees: Iterable[TermOrTypeTree])(implicit ctx: Context): X = (x /: trees)(foldOverTermOrTypeTree)

    def foldOverTree(x: X, tree: Tree)(implicit ctx: Context): X = {
      def localCtx(definition: Definition): Context = definition.symbol.localContext
      tree match {
        case Term.Ident(_) =>
          x
        case Term.Select(qualifier, _, _) =>
          foldTree(x, qualifier)
        case Term.This(qual) =>
          x
        case Term.Super(qual, _) =>
          foldTree(x, qual)
        case Term.Apply(fun, args) =>
          foldTrees(foldTree(x, fun), args)
        case Term.TypeApply(fun, args) =>
          foldTypeTrees(foldTree(x, fun), args)
        case Term.Literal(const) =>
          x
        case Term.New(tpt) =>
          foldTypeTree(x, tpt)
        case Term.Typed(expr, tpt) =>
          foldTypeTree(foldTree(x, expr), tpt)
        case Term.NamedArg(_, arg) =>
          foldTree(x, arg)
        case Term.Assign(lhs, rhs) =>
          foldTree(foldTree(x, lhs), rhs)
        case Term.Block(stats, expr) =>
          foldTree(foldTrees(x, stats), expr)
        case Term.If(cond, thenp, elsep) =>
          foldTree(foldTree(foldTree(x, cond), thenp), elsep)
        case Term.Lambda(meth, tpt) =>
          val a = foldTree(x, meth)
          tpt.fold(a)(b => foldTypeTree(a, b))
        case Term.Match(selector, cases) =>
          foldCaseDefs(foldTree(x, selector), cases)
        case Term.Return(expr) =>
          foldTree(x, expr)
        case Term.Try(block, handler, finalizer) =>
          foldTrees(foldCaseDefs(foldTree(x, block), handler), finalizer)
        case Term.Repeated(elems) =>
          foldTrees(x, elems)
        case Term.Inlined(call, bindings, expansion) =>
          foldTree(foldTrees(x, bindings), expansion)
        case IsDefinition(vdef @ ValDef(_, tpt, rhs)) =>
          implicit val ctx = localCtx(vdef)
          foldTrees(foldTypeTree(x, tpt), rhs)
        case IsDefinition(ddef @ DefDef(_, tparams, vparamss, tpt, rhs)) =>
          implicit val ctx = localCtx(ddef)
          foldTrees(foldTypeTree((foldTrees(x, tparams) /: vparamss)(foldTrees), tpt), rhs)
        case IsDefinition(tdef @ TypeDef(_, rhs)) =>
          implicit val ctx = localCtx(tdef)
          foldTypeTree(x, rhs)
        case IsDefinition(cdef @ ClassDef(_, constr, parents, self, body)) =>
          implicit val ctx = localCtx(cdef)
          foldTrees(foldTrees(foldParents(foldTree(x, constr), parents), self), body)
        case Import(expr, selectors) =>
          foldTree(x, expr)
        case IsPackageClause(clause @ PackageClause(pid, stats)) =>
          foldTrees(foldTree(x, pid), stats)(clause.symbol.localContext)
      }
    }

    def foldOverTypeTree(x: X, tree: TypeOrBoundsTree)(implicit ctx: Context): X = tree match {
      case TypeTree.Synthetic() => x
      case TypeTree.Ident(_) => x
      case TypeTree.Select(qualifier, _) => foldTree(x, qualifier)
      case TypeTree.Project(qualifier, _) => foldTypeTree(x, qualifier)
      case TypeTree.Singleton(ref) => foldTree(x, ref)
      case TypeTree.And(left, right) => foldTypeTree(foldTypeTree(x, left), right)
      case TypeTree.Or(left, right) => foldTypeTree(foldTypeTree(x, left), right)
      case TypeTree.Refined(tpt, refinements) => foldTrees(foldTypeTree(x, tpt), refinements)
      case TypeTree.Applied(tpt, args) => foldTypeTrees(foldTypeTree(x, tpt), args)
      case TypeTree.ByName(result) => foldTypeTree(x, result)
      case TypeTree.Annotated(arg, annot) => foldTree(foldTypeTree(x, arg), annot)
      case TypeBoundsTree(lo, hi) => foldTypeTree(foldTypeTree(x, lo), hi)
    }

    def foldOverCaseDef(x: X, tree: CaseDef)(implicit ctx: Context): X = tree match {
      case CaseDef(pat, guard, body) => foldTree(foldTrees(foldPattern(x, pat), guard), body)
    }

    def foldOverPattern(x: X, tree: Pattern)(implicit ctx: Context): X = tree match {
      case Pattern.Value(v) => foldTree(x, v)
      case Pattern.Bind(_, body) => foldPattern(x, body)
      case Pattern.Unapply(fun, implicits, patterns) => foldPatterns(foldTrees(foldTree(x, fun), implicits), patterns)
      case Pattern.Alternative(patterns) => foldPatterns(x, patterns)
      case Pattern.TypeTest(tpt) => foldTypeTree(x, tpt)
    }

    private def foldOverTermOrTypeTree(x: X, tree: TermOrTypeTree)(implicit ctx: Context): X = tree match {
      case IsTerm(termOrTypeTree) => foldOverTree(x, termOrTypeTree)
      case IsTypeTree(termOrTypeTree) => foldOverTypeTree(x, termOrTypeTree)
    }

  }

   abstract class TreeTraverser extends TreeAccumulator[Unit] {

    def traverseTree(tree: Tree)(implicit ctx: Context): Unit = traverseTreeChildren(tree)
    def traverseTypeTree(tree: TypeOrBoundsTree)(implicit ctx: Context): Unit = traverseTypeTreeChildren(tree)
    def traverseCaseDef(tree: CaseDef)(implicit ctx: Context): Unit = traverseCaseDefChildren(tree)
    def traversePattern(tree: Pattern)(implicit ctx: Context): Unit = traversePatternChildren(tree)

    def foldTree(x: Unit, tree: Tree)(implicit ctx: Context): Unit = traverseTree(tree)
    def foldTypeTree(x: Unit, tree: TypeOrBoundsTree)(implicit ctx: Context) = traverseTypeTree(tree)
    def foldCaseDef(x: Unit, tree: CaseDef)(implicit ctx: Context) = traverseCaseDef(tree)
    def foldPattern(x: Unit, tree: Pattern)(implicit ctx: Context) = traversePattern(tree)

    protected def traverseTreeChildren(tree: Tree)(implicit ctx: Context): Unit = foldOverTree((), tree)
    protected def traverseTypeTreeChildren(tree: TypeOrBoundsTree)(implicit ctx: Context): Unit = foldOverTypeTree((), tree)
    protected def traverseCaseDefChildren(tree: CaseDef)(implicit ctx: Context): Unit = foldOverCaseDef((), tree)
    protected def traversePatternChildren(tree: Pattern)(implicit ctx: Context): Unit = foldOverPattern((), tree)

  }

  abstract class TreeMap { self =>

    def transformTree(tree: Tree)(implicit ctx: Context): Tree = {
      def localCtx(definition: Definition): Context = definition.symbol.localContext
      tree match {
        case IsStatement(tree) => transformStatement(tree)
      }
    }

    def transformStatment(tree: Statment)(implicit ctx: Context): Statement = tree match {
        case IsTerm(tree) => transformTerm(tree)

    }

    def transformTerm(tree: Term)(implicit ctx: Context): Term = {
      tree match {
        case Term.Ident(name) =>
          tree
        case Term.Select(qualifier, name, sig) =>
          Term.Select.copy(tree)(transformTerm(qualifier), name)
        case Term.This(qual) =>
          tree
        case Term.Super(qual, mix) =>
          Term.Super.copy(tree)(transformTerm(qual), mix)
        case Term.Apply(fun, args) =>
          Term.Apply.copy(tree)(transformTerm(fun), transformTerms(args))
        case Term.TypeApply(fun, args) =>
          Term.TypeApply.copy(tree)(transformTerm(fun), transformTypeTrees(args))
        case Term.Literal(const) =>
          tree
//        case New(tpt) =>
//          New.copy(tree)(transform(tpt))
//        case Typed(expr, tpt) =>
//          Typed.copy(tree)(transform(expr), transform(tpt))
//        case NamedArg(name, arg) =>
//          NamedArg.copy(tree)(name, transform(arg))
//        case Assign(lhs, rhs) =>
//          Assign.copy(tree)(transform(lhs), transform(rhs))
        case Term.Block(stats, expr) =>
          Term.Block.copy(tree)(transformStats(stats), transformExpr(expr))
//        case If(cond, thenp, elsep) =>
//          If.copy(tree)(transform(cond), transform(thenp), transform(elsep))
//        case Closure(env, meth, tpt) =>
//          Closure.copy(tree)(transform(env), transform(meth), transform(tpt))
//        case Match(selector, cases) =>
//          Match.copy(tree)(transform(selector), transformSub(cases))
//        case CaseDef(pat, guard, body) =>
//          CaseDef.copy(tree)(transform(pat), transform(guard), transform(body))
//        case Labeled(bind, expr) =>
//          Labeled.copy(tree)(transformSub(bind), transform(expr))
//        case Return(expr, from) =>
//          Return.copy(tree)(transform(expr), transformSub(from))
//        case WhileDo(cond, body) =>
//          WhileDo.copy(tree)(transform(cond), transform(body))
//        case Try(block, cases, finalizer) =>
//          Try.copy(tree)(transform(block), transformSub(cases), transform(finalizer))
//        case SeqLiteral(elems, elemtpt) =>
//          SeqLiteral.copy(tree)(transform(elems), transform(elemtpt))
        case Term.Inlined(call, bindings, expansion) =>
          Term.Inlined.copy(tree)(call, transformSub(bindings), transformTerm(expansion)/*()call.symbol.localContext)*/)
//        case TypeTree() =>
//          tree
//        case SingletonTypeTree(ref) =>
//          SingletonTypeTree.copy(tree)(transform(ref))
//        case AndTypeTree(left, right) =>
//          AndTypeTree.copy(tree)(transform(left), transform(right))
//        case OrTypeTree(left, right) =>
//          OrTypeTree.copy(tree)(transform(left), transform(right))
//        case RefinedTypeTree(tpt, refinements) =>
//          RefinedTypeTree.copy(tree)(transform(tpt), transformSub(refinements))
//        case AppliedTypeTree(tpt, args) =>
//          AppliedTypeTree.copy(tree)(transform(tpt), transform(args))
//        case LambdaTypeTree(tparams, body) =>
//          implicit val ctx = localCtx
//          LambdaTypeTree.copy(tree)(transformSub(tparams), transform(body))
//        case MatchTypeTree(bound, selector, cases) =>
//          MatchTypeTree.copy(tree)(transform(bound), transform(selector), transformSub(cases))
//        case ByNameTypeTree(result) =>
//          ByNameTypeTree.copy(tree)(transform(result))
//        case TypeBoundsTree(lo, hi) =>
//          TypeBoundsTree.copy(tree)(transform(lo), transform(hi))
//        case Bind(name, body) =>
//          Bind.copy(tree)(name, transform(body))
//        case Alternative(trees) =>
//          Alternative.copy(tree)(transform(trees))
//        case UnApply(fun, implicits, patterns) =>
//          UnApply.copy(tree)(transform(fun), transform(implicits), transform(patterns))
//        case EmptyValDef =>
//          tree
//        case tree @ ValDef(name, tpt, _) =>
//          implicit val ctx = localCtx
//          val tpt1 = transform(tpt)
//          val rhs1 = transform(tree.rhs)
//          ValDef.copy(tree)(name, tpt1, rhs1)
//        case tree @ DefDef(name, tparams, vparamss, tpt, _) =>
//          implicit val ctx = localCtx
//          DefDef.copy(tree)(name, transformSub(tparams), vparamss mapConserve (transformSub(_)), transform(tpt), transform(tree.rhs))
//        case tree @ TypeDef(name, rhs) =>
//          implicit val ctx = localCtx
//          TypeDef.copy(tree)(name, transform(rhs))
//        case tree @ Template(constr, parents, self, _) =>
//          Template.copy(tree)(transformSub(constr), transform(parents), transformSub(self), transformStats(tree.body))
//        case Import(expr, selectors) =>
//          Import.copy(tree)(transform(expr), selectors)
//        case PackageDef(pid, stats) =>
//          PackageDef.copy(tree)(transformSub(pid), transformStats(stats)(localCtx))
//        case Annotated(arg, annot) =>
//          Annotated.copy(tree)(transform(arg), transform(annot))
//        case Thicket(trees) =>
//          val trees1 = transform(trees)
//          if (trees1 eq trees) tree else Thicket(trees1)
//        case _ =>
//          tree

      }
    }

    def transformTypeTree(tree: TypeTree)(implicit ctx: Context): TypeTree = {
      ???
    }

    def transformStats(trees: List[Statement])(implicit ctx: Context): List[Statement] =
      trees mapConserve (transformStatement(_))

    def transformTrees(trees: List[Tree])(implicit ctx: Context): List[Tree] =
      trees mapConserve (transformTree(_))

    def transformTerms(trees: List[Term])(implicit ctx: Context): List[Term] =
      trees mapConserve (transformTerm(_))

    def transformTypeTrees(trees: List[TypeTree])(implicit ctx: Context): List[TypeTree] =
      trees mapConserve (transformTypeTree(_))

//    def transformStats(trees: List[Tree])(implicit ctx: Context): List[Tree] =
//      transform(trees)
//    def transform(trees: List[Tree])(implicit ctx: Context): List[Tree] =
//      flatten(trees mapConserve (transform(_)))
//    def transformSub[Tr <: Tree](tree: Tr)(implicit ctx: Context): Tr =
//      transform(tree).asInstanceOf[Tr]
    def transformSub[Tr <: Tree](trees: List[Tr])(implicit ctx: Context): List[Tr] =
      transformTrees(trees).asInstanceOf[List[Tr]]

  }

}
