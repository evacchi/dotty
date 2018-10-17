package scala

package object quoted {

  type Staged[T] = implicit Staging => Expr[T]

  type StagedType[T] = implicit Staging => Type[T]

  implicit class LiftExprOps[T](val x: T) extends AnyVal {
    def toExpr(implicit ev: Liftable[T], st: Staging): Expr[T] = ev.toExpr(x)
  }

}
