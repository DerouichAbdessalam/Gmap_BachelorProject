
abstract class List
case class Cons(head: Int, tail: List) extends List
case object Nil extends List

def getHead(l: List): Int = {
  require(!l.isInstanceOf[Nil])
  l match {
    case Cons(x, _) => x
  }
}

/*  Question for when stainless works again : 
    will this version with equality test work too when stainless reasons about the
    exhaustivnes of the list or will it crash since it only undersands "instances" of 
    ADT's 
    we can easily add a way for equality support in the case of object classes in ADT's
    (just check if we're working with an object class the equ is same as instance of)

*/
def getHead2(l: List): Int = {
  require(l != Nil)
  l match {
    case Cons(x, _) => x
  }
}

//sorting check (easy recursive solution )
def isSorted(l : List) : Boolean = l match {
  case Nil => true
  case Cons(_,Nil) => true
  case Cons(x1, Cons(x2, rest)) =>
    x1 < x2 && isSorted(Cons(x2,rest))
}


//insertion with weak postCondition
def sInsert(x : BigInt, l : List) : List = {
  require(isSorted(l))
  l match {
    case Nil => Cons(x, Nil)
    case Cons(e, rest) if (x == e) => l
    case Cons(e, rest) if (x < e) => Cons(x, Cons(e,rest))
    case Cons(e, rest) if (x > e) => Cons(e, sInsert(x,rest))
  }
} ensuring {(res:List) => isSorted(res)}

   //---------------------------------------------------------------------
//insertion with strong postCondition
//Note : if we use scala lists we can just use the contain methods right ?

def content(l: List): Set[BigInt] = l match {
  case Nil => Set()
  case Cons(i, t) => Set(i) ++ content(t)
}

def sInsert(x : BigInt, l : List) : List = {
  require(isSorted(l))
  l match {
    case Nil => Cons(x, Nil)
    case Cons(e, rest) if (x == e) => l
    case Cons(e, rest) if (x < e) => Cons(x, Cons(e,rest))
    case Cons(e, rest) if (x > e) => Cons(e, sInsert(x,rest))
  }
} ensuring {res =>
   isSorted(res) && content(res) == content(l) ++ Set(x)}

   //---------------------------------------------------------------------