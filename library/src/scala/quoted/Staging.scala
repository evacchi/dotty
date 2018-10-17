package scala.quoted

import scala.tasty.Reflection

import scala.annotation.{implicitAmbiguous, implicitNotFound}

// TODO add @implicitAmbiguous("...")
// TODO add some fake default and give better error message in `Staging` compiler phase
@implicitNotFound("Could not find an implicit Staging.\nIf this is a method that returns an `Expr[T]` you can use `Staged[T]` instead.\n\nQuotedContex is provided inside of top level splices in `inline` macros or within a call to `Toolbox.run`.\n")
trait Staging extends scala.runtime.quoted.Unpickler {
  def show[T](expr: Expr[T]): String
  def show[T](tpe: Type[T]): String

  /** AST reflection API. Provides low level reflection capabilities on the definition of the TASTy trees.
   *
   *  Waring: Using this API can break the static type safty of quotes and splices.
   *          Error will be thown at reflection time.
   */
  val reflection: Reflection
}

object Staging {
   /** Compiler `Staging` available in a top level `~` of an inline macro */
   def macroContext: Staging = throw new Exception("Not in inline macro.")
}
