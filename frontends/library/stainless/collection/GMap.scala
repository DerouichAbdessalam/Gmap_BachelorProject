
package stainless.collection
import stainless.lang._
import stainless.annotation._

  /**
  * defining a map item
  *
  * @param value value of the map item
  * @param present indicates presence of the item in the map
  */
@library
case class MapValue[B](value : B ,present : Boolean)

/**
  * state of the Gmap
  *
  * @param knownItems the items encountered by the map (either present or absent)
  * @param unknownItemInvariant invariant that should hold on the unknown item if present
  * @param length number of known present elements in the map
*/
@library
case class MapState[K, V](knownItems: Map[K,MapValue[V]], unknownItemInvariant : (K, MapValue[V]) => Boolean, length : BigInt)

@extern
def freshSuchThat[T](pred: T => Boolean): T = {
  ??? : T
}.ensuring(pred)


/**
  * Ghost map class implementation
  *
  * @param unknownItem the special item representing all items not yet known by the map
  * @param unknownItemInvariantInit the  unknown item invariant represents the condition that all items not yet known should
  *     satisfy if they're present in our map
  */
@library
class GMap[A, B](unknownItem : (A, MapValue[B]), var mapState: MapState[A,B]){

  /**
    * the length of the map  
    */
  def length  = mapState.length

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
    } ++ Map(key -> MapValue(value, true))
    //this last addition is done in case the key wasn't present in the map


    val newMap = GMap(unknownItem, mapState.unknownItemInvariant)
    newMap.mapState = MapState(newKnownItems, mapState.unknownItemInvariant, newLength)
    newMap
  }

  @extern  
  def setPost(key: A, value: B): Unit = {
    ()
  }.ensuring {
    val newMap = set(key, value)
    //ensure the semantical link between the common elements of the [pre-set map]<->[post-set map]
    forall((keyPrime: A) => (key != keyPrime) ==> (get(keyPrime) == newMap.get(keyPrime)))
  }

  /**
    * creates a new map where the key->value mapping is removed if it was previously present
    *
    * @param key
    * @param value
    * @return the map with the removed mapping
    */
   
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
    } ++ Map(key -> MapValue(freshSuchThat[B]( x => true), false))

    val newMap = GMap(unknownItem, mapState.unknownItemInvariant)
    newMap.mapState = MapState(newKnownItems, mapState.unknownItemInvariant, newLength)
    newMap
  }

  @extern  
  def removePost(key: A): Unit = {
    ()
  }.ensuring {
    val newMap = remove(key)
    //ensure the semantical link between the common elements of the [pre-set map]<->[post-set map]
    forall((keyPrime: A) => (key != keyPrime) ==> (get(keyPrime) == newMap.get(keyPrime)))
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
}

@library
object GMap {
  def apply[A, B](unknownItem : (A, MapValue[B]), unknownItemInvariantInit: (A, MapValue[B]) => Boolean): GMap[A, B] = {
    //the initial map state
    val mapState = MapState[A,B](Map.empty[A, MapValue[B]], unknownItemInvariantInit, 0)

    new GMap(unknownItem, mapState)
  }
}







