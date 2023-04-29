
//use of bigInt to avoid the overflow
object TestMax {
  def max(x: BigInt, y: BigInt): BigInt = {
    val d = x - y
    if (d > 0) x
    else y
  } ensuring(res =>
    x <= res && y <= res && (res == x || res == y))
}

//using restrictions on the input only considering non-negative elems to 
//avoid the Overflow
//Note : Most of the time if a problem is persistant just "restrict yourself" to a safe space 
//       by using some sort of restriction and then start reasoning in that spadce to then 
//       potentially generalize it 
def max(x: Int, y: Int): Int = {
  require(0 < x && 0 < y)
  val d = x - y
  if (d > 0) x
  else y
}ensuring(res =>
  x <= res && y <= res && (res == x || res == y))


