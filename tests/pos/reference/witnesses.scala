class Common {

  trait Ord[T] {
    def compareTo(this x: T)(y: T): Int
    def < (this x: T)(y: T) = x.compareTo(y) < 0
    def > (this x: T)(y: T) = x.compareTo(y) > 0
  }

  trait Convertible[From, To] {
    def convert (this x: From): To
  }

  trait SemiGroup[T] {
    def combine(this x: T)(y: T): T
  }
  trait Monoid[T] extends SemiGroup[T] {
    def unit: T
  }

  trait Functor[F[_]] {
    def map[A, B](this x: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](this x: F[A])(f: A => F[B]): F[B]
    def map[A, B](this x: F[A])(f: A => B) = x.flatMap(f `andThen` pure)

    def pure[A](x: A): F[A]
  }

  inline def summon[T] with (x: T) = x
}

object Witnesses extends Common {

  witness IntOrd for Ord[Int] {
    def compareTo(this x: Int)(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
  }

  witness ListOrd[T] with (ord: Ord[T]) for Ord[List[T]] {
    def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
  }

  witness StringOps {
    def longestStrings(this xs: Seq[String]): Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }

  witness ListOps {
    def second[T](this xs: List[T]) = xs.tail.head
  }

  witness ListMonad for Monad[List] {
    def flatMap[A, B](this xs: List[A])(f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)
  }

  witness ReaderMonad[Ctx] for Monad[[X] => Ctx => X] {
    def flatMap[A, B](this r: Ctx => A)(f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x
  }

  def maximum[T](xs: List[T]) with (cmp: Ord[T]): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T] with (asc: Ord[T]): Ord[T] = new Ord[T] {
    def compareTo(this x: T)(y: T) = asc.compareTo(y)(x)
  }

  def minimum[T](xs: List[T]) with (cmp: Ord[T]) =
    maximum(xs) with descending

  def test(): Unit = {
    val xs = List(1, 2, 3)
    println(maximum(xs))
    println(maximum(xs) with descending)
    println(maximum(xs) with (descending with IntOrd))
    println(minimum(xs))
  }

  case class Context(value: String)
  val c0: Context |=> String = ctx |=> ctx.value
  val c1: (Context |=> String) = (ctx: Context) |=> ctx.value

  class A
  class B
  val ab: (x: A, y: B) |=> Int = (a: A, b: B) |=> 22
}

object PostConditions {
  opaque type WrappedResult[T] = T

  private witness WrappedResult {
    def apply[T](x: T): WrappedResult[T] = x
    def unwrap[T](this x: WrappedResult[T]): T = x
  }

  def result[T] with (wrapped: WrappedResult[T]): T = wrapped.unwrap

  witness {
    def ensuring[T](this x: T)(condition: WrappedResult[T] |=> Boolean): T = {
      assert(condition with WrappedResult(x))
      x
    }
  }
}

object AnonymousWitnesses extends Common {
  witness for Ord[Int] {
    def compareTo(this x: Int)(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
  }

  witness [T: Ord] for Ord[List[T]] {
    def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
  }

  witness {
    def longestStrings(this xs: Seq[String]): Seq[String] = {
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)
    }
  }

  witness {
    def second[T](this xs: List[T]) = xs.tail.head
  }

  witness [From, To] with (c: Convertible[From, To]) for Convertible[List[From], List[To]] {
    def convert (this x: List[From]): List[To] = x.map(c.convert)
  }

  witness for Monoid[String] {
    def combine(this x: String)(y: String): String = x.concat(y)
    def unit: String = ""
  }

  def sum[T: Monoid](xs: List[T]): T =
      xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))
}

object Implicits extends Common {
  implicit object IntOrd extends Ord[Int] {
    def compareTo(this x: Int)(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
  }

  class ListOrd[T: Ord] extends Ord[List[T]] {
    def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
  }
  implicit def ListOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]

  class Convertible_List_List_witness[From, To](implicit c: Convertible[From, To])
  extends Convertible[List[From], List[To]] {
    def convert (this x: List[From]): List[To] = x.map(c.convert)
  }
  implicit def Convertible_List_List_witness[From, To](implicit c: Convertible[From, To])
    : Convertible[List[From], List[To]] =
    new Convertible_List_List_witness[From, To]

  def maximum[T](xs: List[T])
                (implicit cmp: Ord[T]): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T](implicit asc: Ord[T]): Ord[T] = new Ord[T] {
    def compareTo(this x: T)(y: T) = asc.compareTo(y)(x)
  }

  def minimum[T](xs: List[T])(implicit cmp: Ord[T]) =
    maximum(xs)(descending)
}

object Test extends App {
  Witnesses.test()
  import PostConditions._
  val s = List(1, 2, 3).sum
  s.ensuring(result == 6)
}