import scala.quoted._
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    unrolledForeach((x: Int) => 2)
  }
}
