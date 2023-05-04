
package stainless.collection
import stainless.lang._
import stainless.annotation._

  /**
  * defining a map item
  *
  * @param value value of the map item
  * @param present indicates presence of the item in the map
  */
case class MapValue[B](value : B ,present : Boolean)

/**
  * state of the Gmap
  *
  * @param knownItems the items encountered by the map (either present or absent)
  * @param unknownItemInvariant invariant that should hold on the unknown item if present
  * @param length number of known present elements in the map
*/
case class MapState[K, V](knownItems: ListMap[K,MapValue[V]], unknownItemInvariant : (K, MapValue[V]) => Boolean, length : BigInt)

@extern
@pure
def freshSuchThat[T](pred: T => Boolean): T = {
  ??? : T
}.ensuring(pred)

@extern
@pure
def forall[T](pred: T => Boolean): Boolean = {
  ??? : Boolean
}

@extern
@pure
def forallPost[T](pred: T => Boolean, t: T): Unit = {
  require(forall(pred))
}.ensuring(_ => pred(t))

/**
  * Ghost map class implementation
  *
  * @param unknownItem the special item representing all items not yet known by the map
  * @param unknownItemInvariantInit the  unknown item invariant represents the condition that all items not yet known should
  *     satisfy if they're present in our map
  */
class GMap[A, B](unknownItem : (A, MapValue[B]), val unknownItemInvariantInit: (A, MapValue[B]) => Boolean, var mapState: MapState[A,B]){


  // type A = A
  // type B = B

  /**
    * tries to find the value associated with the key given as argument,
    * if the key corresponds to a known item the value, presence pair
    * returned will match the one found. otherwise we return a value, presence pair
    * verifying together with the key the unknown item invariant
    *
    * @param key
    * @returnthe value, presence bollean pair
    */
  def get(key: A): (B, Boolean) = {
    mapState.knownItems.get(key) match {
      case Some(mapValue) => {
        // we generate the two fresh values
        // such that they match the values we found
        // freshV == mapValue.value ; freshP == mapValue.present
        freshSuchThat[(B, Boolean)]{case (freshV, freshP) => freshV == mapValue.value && freshP == mapValue.present}
      }

      case None() => {
        // we generate the two fresh values
        // such that the unkown item invariant applies on the 2 values
        // mapState.unknownItemInvariant(key, freshV, freshP)
        val inv = mapState.unknownItemInvariant
        freshSuchThat[(B, Boolean)]{case (freshV, freshP) => inv(key, MapValue(freshV, freshP))}
      }
    }
  }

  @extern
  @pure
  def getPost(key: A): Unit = {
    ()
  }.ensuring {
    val (value, present) = get(key)
    // Check that the number of unique known items does not exceed its length and the invariants
    mapState.knownItems.filter{case (k, MapValue(_,p)) => p}.size <= mapState.length
  }

  /**
    * creates a new map where the key->value mapping is added
    *
    * @param key
    * @param value
    * @return the map with the added mapping
    */
  @pure
  def set(key: A, value: B): GMap[A, B] = {

    //checking if the element was already present in the map
    val (oldValue, present) = get(key)
    val newLength = if (present) mapState.length else mapState.length + 1

    //no need to create a new state, we just map the old one :
    val newKnownItems = mapState.knownItems.map{
      //case we find the key in the map
      case (k, MapValue(v, p)) if k == key =>
        k -> MapValue(value, true)
      //case the key is different from the key to set
      case x => x
    } + (key -> MapValue(value, true))
    //this last addition is done in case the key wasn't present in the map


    val newMap = GMap(unknownItem, mapState.unknownItemInvariant)
    newMap.mapState = MapState(newKnownItems, mapState.unknownItemInvariant, newLength)
    newMap
  }

  @extern
  @pure
  def setPost(key: A, value: B): Unit = {
    ()
  }.ensuring {
    val newMap = set(key, value)
    //ensure the semantical link between the common elements of the [pre-set map]<->[post-set map]
    def pred(keyPrime: A): Boolean = (key != keyPrime) ==> (get(keyPrime) == newMap.get(keyPrime))
    forall(pred) && newMap.get(key) == (value, true)
  }

  /**
    * creates a new map where the key->value mapping is removed if it was previously present
    *
    * @param key
    * @param value
    * @return the map with the removed mapping
    */
  @pure
  def remove(key: A): GMap[A, B] = {

    //checking if the element was already present in the map
    val (value, present) = get(key)
    val newLength = mapState.length + (if (present) -1 else 0)

    val newKnownItems = mapState.knownItems.map{
      //case we encounter the key : associate the key with a fresh value and false presence
      case (k, MapValue(v, p)) if (k == key)  =>
        //the choose creates a fresh value
        k -> MapValue(freshSuchThat[B](x => true), false)
      //otherwise keep the element intact
      case x => x
    } + (key -> MapValue(freshSuchThat[B]( x => true), false))

    val newMap = GMap(unknownItem, mapState.unknownItemInvariant)
    newMap.mapState = MapState(newKnownItems, mapState.unknownItemInvariant, newLength)
    newMap
  }

  @extern
  @pure
  def removePost(key: A): Unit = {
    ()
  }.ensuring {
    val newMap = remove(key)
    //ensure the semantical link between the common elements of the [pre-set map]<->[post-set map]
    def pred(keyPrime: A): Boolean = (key != keyPrime) ==> (get(keyPrime) == newMap.get(keyPrime))
    forall(pred)
  }

  /**
    * checks if property is verified by all present elements in the map
    *
    * @param predicate the property to check
    * @return whether the predicate holds for all present elements or not
    */
  def forAll(predicate: (A, MapValue[B]) => Boolean): Boolean = {
    // check predicate for present known items
    val knownPredicateHolds =  mapState.knownItems.filter{
        case k -> MapValue(v, p) => p
    }.forall{case k -> mapValue => predicate(k,mapValue)}

    // check if the predicate holds for the unknown item
    val (unknownkey , MapValue(unknownValue, unknownPresence)) = unknownItem
    val unknownPredicateHolds = if (unknownPresence) predicate(unknownkey, unknownItem._2) else true

    // update the map's invariant
    val inv = mapState.unknownItemInvariant
    mapState =  MapState(mapState.knownItems, (unknownKey, unknownValue) => inv(unknownKey, unknownValue) && unknownPredicateHolds, mapState.length)

    //the result of the forAll operation
    knownPredicateHolds && unknownPredicateHolds
  }

  def forAllPost(predicate: (A, MapValue[B]) => Boolean): Unit = {
    ()
  }.ensuring {
    val thisNew = freshCopy(this)
    val res = thisNew.forAll(predicate)
    this.mapState.knownItems == thisNew.mapState.knownItems
  }
}

object GMap {
  def apply[A, B](unknownItem : (A, MapValue[B]), unknownItemInvariantInit: (A, MapValue[B]) => Boolean): GMap[A, B] = {
    //the initial map state
    val mapState = MapState[A,B](Map.empty[A, MapValue[B]], unknownItemInvariantInit, 0)

    new GMap(unknownItem, unknownItemInvariantInit, mapState)
  }
  def test(unknownItem : (Int, MapValue[Int]), arrayInvariant: (Int, MapValue[Int]) => Boolean): Unit = {
    val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant)
    val nA = arrayMap.set(0, 1)
    arrayMap.setPost(0,1)
  }

  def test2(x: BigInt): Unit = {
    val unknownItem = (42, MapValue(42, false))
    val arrayInvariant = (k: Int, mv: MapValue[Int]) => k == mv.value
    val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant)
    //setting the 42->42 mapping + verifying propreties
    val arrayMap2 = arrayMap.set(42, 42)
    arrayMap.setPost(42, 42)
    

    val (got, present) = arrayMap2.get(42)
    arrayMap2.getPost(42)
    assert(got == 42) // OK
    
    // get on absent value returns a value verifying the unknown invariant 
    val (got2, present2) = arrayMap.get(50)
    assert(arrayInvariant(50, MapValue(got2, present2))) 
    assert(got2 == 50)

  //------------------------------------------------------------------------------------
    // Sur la nouvelle
    // Ne passe pas, parce que newKnownItems dans set utilise map et ++
    // qui sont non contraint (c-a-d Stainless peut lui donner des valeurs arbitraires)
    val (got3, present3) = arrayMap2.get(50)
    assert(arrayInvariant(50, MapValue(got3, present3)))
    assert(got3 == 50)
  //------------------------------------------------------------------------------------

  }
}