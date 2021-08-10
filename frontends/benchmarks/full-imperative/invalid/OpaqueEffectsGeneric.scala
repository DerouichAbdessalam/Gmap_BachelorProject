/*
This benchmark is important because it shows a nice counterexample.

However, frame condition checks time out, which is why a CI might
not realize that there is a properly failing counterexample.
*/

  import stainless.annotation._
import stainless.collection._
import stainless.lang._
import stainless.lang.Option._
import stainless.lang.StaticChecks._
import stainless.proof.check

object TreeImmutMapGenericExample {
  case class Cell[T](var value: T) extends AnyHeapRef

  case class Leaf[T](data: Cell[T]) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  sealed abstract class Tree[T] {

    @ghost def repr: Set[AnyHeapRef] =
      this match {
        case Leaf(data) => Set[AnyHeapRef](data)
        case Branch(left, right) => left.repr ++ right.repr
        //totry: Set[AnyHeapRef]() 
      }
    @opaque
    def tmap(f: T => T): Unit = {
      reads(repr)
      modifies(repr)
      decreases(this)

      this match {
        case Leaf(data) =>
          data.value = f(data.value)
        case Branch(left, right) =>
          left.tmap(f)
          right.tmap(f)
      }
    }
  }
  /* gives counterexample, such as:

[info] [Warning ] Found counter-example:
[info] [Warning ]   t: Tree[Int]                -> Leaf[Int](HeapRef(4))
[info] [Warning ]   c: HeapRef                  -> HeapRef(4)
[info] [Warning ]   heap0: Map[HeapRef, Object] -> {HeapRef(4) -> Cell(Cell[Object](SignedBitvector32(0))), * -> SignedBitvector32(2)}

*/

  def test(t: Tree[Int], c: Cell[Int]) = {
    require(c.value == 0)
    reads(t.repr ++ Set[AnyHeapRef](c))
    modifies(t.repr)
    
    t.tmap(x => (x | 1))
  } ensuring(_ => c.value == 0)
}