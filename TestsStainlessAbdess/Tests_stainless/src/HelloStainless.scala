import stainless.collection._

object HelloStainless {
  def myTail(xs: List[BigInt]): BigInt = {
    require(xs.nonEmpty)
    xs match {
      case Cons(h, _) => h
      // Match provably exhaustive
    }
  }
}


