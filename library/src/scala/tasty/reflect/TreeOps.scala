package scala.tasty
package reflect

trait TreeOps extends Core {

  trait TreeAPI {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position

    def symbol(implicit ctx: Context): Symbol
  }
  implicit def TreeDeco(tree: Tree): TreeAPI

  val IsPackageClause: IsPackageClauseModule
  abstract class IsPackageClauseModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageClause]
  }

  val PackageClause: PackageClauseModule
  abstract class PackageClauseModule {
    def apply(pid: Ref, stats: List[Tree])(implicit ctx: Context): PackageClause
    def unapply(tree: Tree)(implicit ctx: Context): Option[(Ref, List[Tree])]
  }

  trait PackageClauseAPI {

  }
  implicit def PackageClauseDeco(pack: PackageClause): PackageClauseAPI

  // ----- Statements -----------------------------------------------

  val Import: ImportModule
  abstract class ImportModule {
    def apply(expr: Term, selectors: List[ImportSelector])(implicit ctx: Context): Import
    def unapply(imp: Tree)(implicit ctx: Context): Option[(Term, List[ImportSelector])]
  }

  trait ImportAPI {
    def expr(implicit ctx: Context): Term
    def selector(implicit ctx: Context): List[ImportSelector]
  }
  implicit def ImportDeco(imp: Import): ImportAPI

  // ----- Definitions ----------------------------------------------

  val IsDefinition: IsDefinitionModule
  abstract class IsDefinitionModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[Definition]
  }

  trait DefinitionAPI {
    def name(implicit ctx: Context): String
  }
  implicit def DefinitionDeco(definition: Definition): DefinitionAPI

  // ClassDef

  val IsClassDef: IsClassDefModule
  abstract class IsClassDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ClassDef]
  }

  val ClassDef: ClassDefModule
  abstract class ClassDefModule {
    def apply(name: String, constr: DefDef, parents: List[TermOrTypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[TermOrTypeTree], Option[ValDef], List[Statement])]
  }

  trait ClassDefAPI {
    def constructor(implicit ctx: Context): DefDef
    def parents(implicit ctx: Context): List[TermOrTypeTree]
    def self(implicit ctx: Context): Option[ValDef]
    def body(implicit ctx: Context): List[Statement]

    def symbol(implicit ctx: Context): ClassSymbol
  }
  implicit def ClassDefDeco(cdef: ClassDef): ClassDefAPI

  // DefDef

  val IsDefDef: IsDefDefModule
  abstract class IsDefDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[DefDef]
  }

  val DefDef: DefDefModule
  abstract class DefDefModule {
    def apply(symbol: DefSymbol, rhsFn: List[Type] => List[List[Term]] => Option[Term])(implicit ctx: Context): DefDef
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])]
  }

  trait DefDefAPI {
    def typeParams(implicit ctx: Context): List[TypeDef]
    def paramss(implicit ctx: Context): List[List[ValDef]]
    def returnTpt(implicit ctx: Context): TypeTree
    def rhs(implicit ctx: Context): Option[Term]

    def symbol(implicit ctx: Context): DefSymbol
  }
  implicit def DefDefDeco(ddef: DefDef): DefDefAPI

  // ValDef

  val IsValDef: IsValDefModule
  abstract class IsValDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[ValDef]
  }

  val ValDef: ValDefModule
  abstract class ValDefModule {
    def apply(sym: ValSymbol, rhs: Option[Term])(implicit ctx: Context): ValDef
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeTree, Option[Term])]
  }

  trait ValDefAPI {
    def tpt(implicit ctx: Context): TypeTree
    def rhs(implicit ctx: Context): Option[Term]

    def symbol(implicit ctx: Context): ValSymbol
  }
  implicit def ValDefDeco(vdef: ValDef): ValDefAPI

  // TypeDef

  val IsTypeDef: IsTypeDefModule
  abstract class IsTypeDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[TypeDef]
  }

  val TypeDef: TypeDefModule
  abstract class TypeDefModule {
    def apply(symbol: TypeSymbol)(implicit ctx: Context): TypeDef
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, TypeOrBoundsTree /* TypeTree | TypeBoundsTree */)]
  }

  trait TypeDefAPI {
    def rhs(implicit ctx: Context): TypeOrBoundsTree
    def symbol(implicit ctx: Context): TypeSymbol
  }
  implicit def TypeDefDeco(tdef: TypeDef): TypeDefAPI

  // PackageDef

  val IsPackageDef: IsPackageDefModule
  abstract class IsPackageDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[PackageDef]
  }

  trait PackageDefAPI {
    def owner(implicit ctx: Context): PackageDef
    def members(implicit ctx: Context): List[Statement]
    def symbol(implicit ctx: Context): PackageSymbol
  }
  implicit def PackageDefDeco(pdef: PackageDef): PackageDefAPI

  val PackageDef: PackageDefModule
  abstract class PackageDefModule {
    def unapply(tree: Tree)(implicit ctx: Context): Option[(String, PackageDef)]
  }

  // ----- Terms ----------------------------------------------------

  trait TermAPI {
    def tpe(implicit ctx: Context): Type
    def pos(implicit ctx: Context): Position
    def underlyingArgument(implicit ctx: Context): Term
    def underlying(implicit ctx: Context): Term
  }
  implicit def TermDeco(term: Term): TermAPI

  val IsTerm: IsTermModule
  abstract class IsTermModule {
    /** Matches any term */
    def unapply(tree: Tree)(implicit ctx: Context): Option[Term]
    /** Matches any term */
    def unapply(parent: TermOrTypeTree)(implicit ctx: Context, dummy: DummyImplicit): Option[Term]
  }

  /** Scala term. Any tree that can go in expression position. */
  val Term: TermModule
  abstract class TermModule {

    val Ref: RefModule
    abstract class RefModule {

      /** Create a reference tree */
      def apply(sym: Symbol)(implicit ctx: Context): Ref

      def copy(original: Tree)(name: String)(implicit ctx: Context): Ref

    }

    /** Scala term identifier */
    val Ident: IdentModule
    abstract class IdentModule {

      /** Create a term identifier */
      def apply(name: String)(implicit ctx: Context): Ident

      def copy(original: Tree)(name: String)(implicit ctx: Context): Ident

      /** Matches a term identifier and returns its name */
      def unapply(tree: Tree)(implicit ctx: Context): Option[String]

    }

    /** Scala term selection */
    val Select: SelectModule
    abstract class SelectModule {

      /** Create a term selection `<qual: Term>.<name: String>: <signature: Signature>` */
      def apply(qualifier: Term, name: String, signature: Option[Signature])(implicit ctx: Context): Select

      def copy(original: Tree)(qualifier: Term, name: String)(implicit ctx: Context): Select

      /** Matches `<qualifier: Term>.<name: String>: <signature: Signature>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, String, Option[Signature])]

    }

    /** Scala literal constant */
    val Literal: LiteralModule
    abstract class LiteralModule {

      /** Create a literal constant */
      def apply(constant: Constant)(implicit ctx: Context): Literal

      def copy(original: Tree)(constant: Constant)(implicit ctx: Context): Literal

      /** Matches a literal constant */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Constant]

    }

    /** Scala `this` or `this[id]` */
    val This: ThisModule
    abstract class ThisModule {

      /** Create a `this[<id: Id]>` */
      def apply(cls: ClassSymbol)(implicit ctx: Context): This

      def copy(original: Tree)(qual: Option[Id]): This

      /** Matches `this[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Option[Id]]

    }

    /** Scala `new` */
    val New: NewModule
    abstract class NewModule {

      /** Create a `new <tpt: TypeTree>` */
      def apply(tpt: TypeTree)(implicit ctx: Context): New

      def copy(original: Tree)(tpt: TypeTree)(implicit ctx: Context): New

      /** Matches a `new <tpt: TypeTree>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[TypeTree]

    }

    /** Scala named argument `x = y` in argument position */
    val NamedArg: NamedArgModule
    abstract class NamedArgModule {

      /** Create a named argument `<name: String> = <value: Term>` */
      def apply(name: String, arg: Term)(implicit ctx: Context): NamedArg

      def copy(tree: NamedArg)(name: String, arg: Term)(implicit ctx: Context): NamedArg

      /** Matches a named argument `<name: String> = <value: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Term)]

    }

    /** Scala parameter application */
    val Apply: ApplyModule
    abstract class ApplyModule {

      /** Create a function application `<fun: Term>(<args: List[Term]>)` */
      def apply(fn: Term, args: List[Term])(implicit ctx: Context): Apply

      def copy(original: Tree)(fun: Term, args: List[Term])(implicit ctx: Context): Apply

      /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[Term])]

    }

    /** Scala type parameter application */
    val TypeApply: TypeApplyModule
    abstract class TypeApplyModule {

      /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def apply(fn: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply

      def copy(original: Tree)(fun: Term, args: List[TypeTree])(implicit ctx: Context): TypeApply

      /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[TypeTree])]

    }

    /** Scala `x.super` or `x.super[id]` */
    val Super: SuperModule
    abstract class SuperModule {

      /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
      def apply(qual: Term, mix: Option[Id])(implicit ctx: Context): Super

      def copy(original: Tree)(qual: Term, mix: Option[Id]): Super

      /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[Id])]

    }

    /** Scala ascription `x: T` */
    val Typed: TypedModule
    abstract class TypedModule {

      /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
      def apply(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed

      def copy(original: Tree)(expr: Term, tpt: TypeTree)(implicit ctx: Context): Typed

      /** Matches a type ascription `<x: Term>: <tpt: TypeTree>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, TypeTree)]

    }

    /** Scala assign `x = y` */
    val Assign: AssignModule
    abstract class AssignModule {

      /** Create an assignment `<lhs: Term> = <rhs: Term>` */
      def apply(lhs: Term, rhs: Term)(implicit ctx: Context): Assign

      def copy(original: Tree)(lhs: Term, rhs: Term)(implicit ctx: Context): Assign

      /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]

    }

    /** Scala code block `{ stat0; ...; statN; expr }` term */
    val Block: BlockModule
    abstract class BlockModule {

      /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def apply(stats: List[Statement], expr: Term)(implicit ctx: Context): Block

      def copy(original: Tree)(stats: List[Statement], expr: Term)(implicit ctx: Context): Block

      /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Statement], Term)]

    }

    val Lambda: LambdaModule
    abstract class LambdaModule {

      def apply(meth: Term, tpt: Option[TypeTree])(implicit ctx: Context): Lambda

      def copy(original: Tree)(meth: Tree, tpt: Tree)(implicit ctx: Context): Lambda

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Option[TypeTree])]

    }

    /** Scala `if`/`else` term */
    val If: IfModule
    abstract class IfModule {

      /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def apply(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If

      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term)(implicit ctx: Context): If

      /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term, Term)]

    }

    /** Scala `match` term */
    val Match: MatchModule
    abstract class MatchModule {

      /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def apply(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match

      def copy(original: Tree)(selector: Term, cases: List[CaseDef])(implicit ctx: Context): Match

      /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef])]

    }

    /** Scala `try`/`catch`/`finally` term */
    val Try: TryModule
    abstract class TryModule {

      /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try

      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term])(implicit ctx: Context): Try

      /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, List[CaseDef], Option[Term])]

    }

    /** Scala local `return` */
    val Return: ReturnModule
    abstract class ReturnModule {

      /** Creates `return <expr: Term>` */
      def apply(expr: Term)(implicit ctx: Context): Return

      def copy(original: Tree)(expr: Term)(implicit ctx: Context): Return

      /** Matches `return <expr: Term>` */
      def unapply(tree: Tree)(implicit ctx: Context): Option[Term]

    }

    /** Scala repeated arguments */
    val Repeated: RepeatedModule
    abstract class RepeatedModule {

      def apply(elems: List[Term])(implicit ctx: Context): Repeated

      def copy(original: Tree)(elems: List[Term])(implicit ctx: Context): Repeated

      def unapply(tree: Tree)(implicit ctx: Context): Option[List[Term]]

    }

    val Inlined: InlinedModule
    abstract class InlinedModule {

      def apply(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined

      def copy(original: Tree)(call: Option[TermOrTypeTree], bindings: List[Definition], expansion: Term)(implicit ctx: Context): Inlined

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Option[TermOrTypeTree], List[Definition], Term)]

    }

    val SelectOuter: SelectOuterModule
    abstract class SelectOuterModule {

      def apply(qualifier: Term, levels: Int, tpe: Type)(implicit ctx: Context): SelectOuter

      def copy(original: Tree)(qualifier: Term, levels: Int, tpe: Type)(implicit ctx: Context): SelectOuter

      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Int, Type)]

    }

    val While: WhileModule
    abstract class WhileModule {

      /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
      def apply(cond: Term, body: Term)(implicit ctx: Context): While

      def copy(original: Tree)(cond: Term, body: Term)(implicit ctx: Context): While

      /** Matches a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(tree: Tree)(implicit ctx: Context): Option[(Term, Term)]

    }
  }

  implicit def termAsTermOrTypeTree(term: Term): TermOrTypeTree
}
