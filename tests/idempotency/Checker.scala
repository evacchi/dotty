
object Test {
  def main(args: Array[String]): Unit =
    IdempotencyCheck.checkIdempotency("../out/idempotency")
}
