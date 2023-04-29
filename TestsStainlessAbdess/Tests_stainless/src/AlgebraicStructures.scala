//Note : the typeclasses course was verry interesting in Funprog, I remember 
//it was particularly useful in implicit parametres if i remember correctly

//example given in tutorial 
//------------------------------------Monads-----------------------------------
abstract class Monoid[A] {
  def empty: A
  def append(x: A, y: A): A

  @law
  def law_leftIdentity(x: A) = {
    append(empty, x) == x
  }

  @law
  def law_rightIdentity(x: A) = {
    append(x, empty) == x
  }

  @law
  def law_associativity(x: A, y: A, z: A) = {
    append(x, append(y, z)) == append(append(x, y), z)
  }
}

implicit val bigIntAdditiveMonoid: Monoid[BigInt] = new Monoid[BigInt] {
  override def empty: BigInt = 0
  override def append(x: BigInt, y: BigInt): BigInt = x + y
}
//------------------------------------Monads-----------------------------------


//------------------------------------ Groups -------------------------------
//For groups the main thing i should look for is how to prove existence for the
//invertability property : TODO
//idea : i think i saw in the stainless github a way to define forAll, negate the proposition
//to make the forAll appear then use it  
abstract class Group[A] {
  def empty: A
  def append(x: A, y: A): A

  @law
  def law_leftIdentity(x: A) = {
    append(empty, x) == x
  }

  @law
  def law_rightIdentity(x: A) = {
    append(x, empty) == x
  }

  @law
  def law_associativity(x: A, y: A, z: A) = {
    append(x, append(y, z)) == append(append(x, y), z)
  }
  
  @law
  def law_associativity(x: A, y: A, z: A) = {
    append(x, append(y, z)) == append(append(x, y), z)
  }
  
  //---------------------->
  /*
  @law
  def inverse_existance(x: A, y: A, z: A) = {
    append(x, append(y, z)) == append(append(x, y), z)
  }
  */
  //<----------------------
}

//Commutative group definition 
abstract class CommGroups[A] extends Group[A]{
  @law
  def Commuatativity(x: A, y: A) = {
    append(x, y) == append(y, x)
  }

}


//------------------------------------ Groups -------------------------------



//OK so basically Nat is what we call an inductive data type 
//(seen previously in a Funprog lab if i remember corectly)
//i.e all elements are be genereated by induction from the first elem 

sealed abstract class Nat {
  def +(m: Nat): Nat = this match {
    case Zero => m
    case Succ(n) => Succ(n + m)
  }

  def *(m: Nat): Nat = this match {
    case Zero => Zero
    case Succ(n) => n * m + m
  }
}

final case object Zero extends Nat
final case class Succ(prev: Nat) extends Nat

//we instantiate the monoid typeclass with the Nat type
//Apparently stainless isn't able to reason about (verify the laws 
//that we gave previously) an instantiation of the MOnoid typeclass for this 
/*
like this definition wouldn't work 
:::::
implicit def natAdditiveMonoid: Monoid[Nat] = new Monoid[Nat] {
  def empty: nat = Zero
  def append(x: Nat, y: Nat): Nat = x + y
}
:::::
*/
//<------------------------Helping Stainless------------------------------------------->

@induct
def lemma_rightIdentity_plus(x: Nat): Boolean = {
  x + Zero == x
}.holds

@induct
def lemma_associativity_plus(x: Nat, y: Nat, z: Nat): Boolean = {
  x + (y + z) == (x + y) + z
}.holds

//<------------------------------------------------------------------->

implicit def natAdditiveMonoid: Monoid[Nat] = new Monoid[Nat] {
  def empty: nat = Zero
  def append(x: Nat, y: Nat): Nat = x + y


//After helping stainless this is the way to tell it to user our help
//we override the original law definition to tell stainless that we are using the  
//lemmas to prove the original laws using the "because" function 
  override def law_rightIdentity(x: Nat) = {
    super.law_rightIdentity(x) because lemma_rightIdentity_plus(x)
  }

  override def law_associativity(x: Nat, y: Nat, z: Nat) = {
    super.law_associativity(x, y, z) because lemma_associativity_plus(x, y, z)
  }
}

//-------------------------------------------------------------------



//PROBLEM : stainless doesn't stop you from defining 2 implicit parametres in the same scope 
//in the following case we can define 2 implicit parametres with the same type :
//Monoid[BigInt] which as seen in FunProg will cause a problem 
//just be careful to never define such values, instead if you find yourself defining a Typeclass dataType
//instantiated with a type that can have multiple Typeclasse definition from it (BigInt example 
//with + and * definitions for combine) put the type (BigInt here) in a wrapper class
//---------------------> see solution of the problem 

abstract class Monoid2[A] {
  def empty: A
  def combine(x: A, y: A): A
}

implicit val bigIntAddMonoid: Monoid2[BigInt] = new Monoid2[BigInt] {
  override def empty: BigInt = 0
  override def combine(x: BigInt, y: BigInt): BigInt = x + y
}

implicit val bigIntProdMonoid: Monoid2[BigInt] = new Monoid2[BigInt] {
  override def empty: BigInt = 1
  override def combine(x: BigInt, y: BigInt): BigInt = x * y
}

def fold[A](list: List[A])(implicit M: Monoid[A]): A = {
  list.foldRight(M.empty)(M.combine)
}

val list: List[BigInt] = List(2, 3)
val res: BigInt = fold(list) 

//----------------------->Solution of ambiguity problem 
case class Sum(value: BigInt)     extends AnyVal
case class Product(value: BigInt) extends AnyVal

implicit val bigIntSumMonoid: Monoid[Sum] = new Monoid[Sum] {
  override def empty: Sum = Sum(0)
  override def combine(x: Int, y: Int): Sum = Sum(x.value + y.value)
}

implicit val bigIntProductMonoid: Monoid[Product] = new Monoid[Product] {
  override def empty: Product = Product(1)
  override def combine(x: Int, y: Int): Product = Product(x.value * y.value)
}
//--------------------------------------------------------


