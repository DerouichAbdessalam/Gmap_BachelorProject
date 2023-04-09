
package stainless.collection
import scala.math.BigInt
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
case class MapState[K, V](knownItems: scala.collection.Map[K,MapValue[V]], unknownItemInvariant : (K, MapValue[V]) => Boolean, length : BigInt)


/**
  * Ghost map class implementation
  *
  * @param unknownItem the special item representing all items not yet known by the map 
  * @param unknownItemInvariantInit the  unknown item invariant represents the condition that all items not yet known should 
  *     satisfy if they're present in our map  
  */
class GMap[A, B](unknownItem : (A, MapValue[B]), unknownItemInvariantInit : (A, MapValue[B]) => Boolean){
  
  type Key = A
  type Value = B
  
  //the map state 
  var mapState = MapState[Key, Value](scala.collection.Map.empty[Key, MapValue[B]], unknownItemInvariantInit, 0)
  
  /**
    * tries to find the value associated with the key given as argument, 
    * if the key corresponds to a known item the value, presence pair 
    * returned will match the one found. otherwise we return a value, presence pair
    * verifying together with the key the unknown item invariant
    *
    * @param key 
    * @returnthe value, presence bollean pair
    */
  def get(key: Key): (Value, Boolean) = {
    mapState.knownItems.get(key) match {
      case scala.Some(mapValue) => {
        // we generate the two fresh values 
        // such that they match the values we found 
        // freshV == mapValue.value ; freshP == mapValue.present      
        choose[(Value, Boolean)]{case (freshV, freshP) => freshV == mapValue.value && freshP == mapValue.present}
      }

      case scala.None => {
        // we generate the two fresh values 
        // such that the unkown item invariant applies on the 2 values 
        // mapState.unknownItemInvariant(key, freshV, freshP)
        choose[(Value, Boolean)]{case (freshV, freshP) => mapState.unknownItemInvariant(key, MapValue(freshV, freshP))}
      }
    }
  }ensuring{
    case (value, present) => 
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
  def set(key: Key, value: Value): GMap[A, B] = {

    //checking if the element was already present in the map
    val (oldValue, present) = get(key)
    val newLength = if (present) mapState.length else mapState.length + 1

    //no need to create a new state, we just map the old one : 
    val newKnownItems = mapState.knownItems.map {
      //case we find the key in the map
      case (k, MapValue(v, p)) if k == key =>
        k -> MapValue(value, true)
      //case the key is different from the key to set  
      case x => x
    } ++ scala.collection.Map(key -> MapValue(value, true))
    //this last addition is done in case the key wasn't present in the map 
    
     
    val newMap = new GMap(unknownItem, mapState.unknownItemInvariant)
    newMap.mapState = MapState(newKnownItems, mapState.unknownItemInvariant, newLength)
    newMap
  }ensuring{newMap => 

    //ensure the semantical link between the common elements of the [pre-set map]<->[post-set map]     
    forall((keyPrime: Key) => (key != keyPrime) ==> (get(keyPrime) == newMap.get(keyPrime)))
  }



  /**
    * creates a new map where the key->value mapping is removed if it was previously present
    *
    * @param key
    * @param value
    * @return the map with the removed mapping 
    */
  def remove(key: Key): GMap[Key, Value] = {

    //checking if the element was already present in the map
    val (value, present) = get(key)
    val newLength = mapState.length + (if (present) -1 else 0)

    val newKnownItems = mapState.knownItems.map{ 
      //case we encounter the key : associate the key with a fresh value and false presence
      case (k, MapValue(v, p)) if (k == key)  =>
        //the choose creates a fresh value 
        k -> MapValue(choose[Value](x => true), false) 
      //otherwise keep the element intact 
      case x => x
    } ++ scala.collection.Map(key -> MapValue(choose[Value]( x => true), false)) 

    val newMap = new GMap(unknownItem, mapState.unknownItemInvariant)
    newMap.mapState = MapState(newKnownItems, mapState.unknownItemInvariant, newLength)
    newMap
  }.ensuring{newMap => 

    //ensure the semantical link between the common elements of the [pre-set map]<->[post-set map]     
    forall((keyPrime: Key) => (key != keyPrime) ==> (get(keyPrime) == newMap.get(keyPrime)))
  }

  /**
    * checks if property is verified by all present elements in the map 
    * 
    * @param predicate the property to check 
    * @return whether the predicate holds for all present elements or not 
    */
  def forAll(predicate: (Key, MapValue[Value]) => Boolean): Boolean = {
    // check predicate for present known items
    val knownPredicateHolds =  mapState.knownItems.filter{
        case k -> MapValue(v, p) => p
    }.forall{case k -> mapValue => predicate(k,mapValue)}

    // check if the predicate holds for the unknown item
    val (unknownkey , MapValue(unknownValue, unknownPresence)) = unknownItem
    val unknownPredicateHolds = if (unknownPresence) predicate(unknownkey, unknownItem._2) else true 

    // update the map's invariant
    val newInvariant = (unknownKey : Key , unknownValue : MapValue[Value]) => mapState.unknownItemInvariant(unknownKey, unknownValue) && unknownPredicateHolds
    
    mapState =  MapState(mapState.knownItems, newInvariant, mapState.length)

    //the result of the forAll operation
    knownPredicateHolds && unknownPredicateHolds
  }
}
  











    

