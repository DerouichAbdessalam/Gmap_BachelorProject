
package stainless.collection
import org.scalacheck.Arbitrary
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
case class MapState[K, V](knownItems: Map[K,MapValue[V]], unknownItemInvariant : (K, MapValue[V]) => Boolean, length : BigInt)


/**
  * Ghost map class implementation
  *
  * @param uknownItem the special item representing all items not yet known by the map 
  * @param unknownItemInvariant the  unknown item invariant represents the condition that all items not yet known should 
  *     satisfy if they're present in our map  
  */
class GMap[A, B](uknownItem : (Key, MapValue[B]), unknownItemInvariant : (Key, MapValue[B]) => Boolean){
  
  type Key = A
  type Value = B
  
  //the map state 
  var mapState = MapState[B](Map.Empty[Key, MapValue[B]], unknownItemInvariant, 0)
  
  /**
    * tries to find the value associated with the key given as argument, 
    * if the key corresponds to a known item the value, presence pair 
    * returned will match the one found. otherwise we return a value, presence pair
    * verifying together with the key the uknown item invariant
    *
    * @param key 
    * @returnthe value, presence bollean pair
    */
  def get(key: Key): (Value, Boolean) = {
    mapState.knownItems().get(key) match {
      case None => {
        // we generate the two fresh values 
        // such that the unkown item invariant applies on the 2 values 
        // mapState.unknownItemInvariant(key, freshV, freshP)
        choose[(Value, Boolean)](t => mapState.unknownItemInvariant(key, MapValue(t._1, t._2)))
      }
      case Some(mapValue) => {
        // we generate the two fresh values 
        // such that they match the values we found 
        // freshV == mapValue.value ; freshP == mapValue.present      
        choose[(Value, Boolean)]((freshV, freshP) => freshV == mapValue.value && freshP == mapValue.present)
      }
    }
  }ensuring{
    case (value, present) => 
      // Check that the number of unique known items does not exceed its length and the invariants 
      mapState.knownItems.filter{case MapItem(_,_,p) => p}.size <= mapState.length

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
    } + (key -> MapValue(value, true))
    //this last addition is done in case the key wasn't present in the map 
    
     
    val newMap = GMap(uknownItem, unknownItemInvariant)
    newMap.mapState = MapState(newKnownItems, unknownItemInvariant, newLength)
    
  }ensuring{newMap => 

    //ensure the semantical link between the elements of the [pre-set map]<->[post-set map]     
    forall((keyPrime: Key) => (key != keyPrime) ==> (get(map, keyPrime) == get(newMap, keyPrime)))
  }



  /**
    * creates a new map where the key->value mapping is removed if it was previously present
    *
    * @param key
    * @param value
    * @return the map with the removed mapping 
    */
  def remove(key: Key): GMap[A, B] = {

    //checking if the element was already present in the map
    val (value, present) = get(key)
    val newLength = mapState.length + (if (present) -1 else 0)

    val newKnownItems = mapState.knownItems.map{ 
      //case we encounter the key : associate the key with a fresh value and false presence
      case (k, MapValue(v, p)) if (k == key)  =>
        k -> MapValue(choose[Value], false) 
      //otherwise keep the element intact 
      case x => x
    } +(k -> MapValue(choose[Value], false)) 

    GMap[A, B](MapValue[B](value, false), mapState.unknownItemInvariant) ensuring { gmap =>
      gmap.mapState.length == newLength &&
      gmap.mapState.knownItems.keySet == newKnownItems.keySet &&
      gmap.mapState.knownItems.forall{ case (k, MapValue(v, p)) =>
        newKnownItems.get(k).contains(MapValue(v, p))
      } &&
      getItemInvariants(gmap.mapState.unknownItemInvariant)
    }
  }

/**
  * should the layers be explicitely handeled and managed like in this example ? 
  */
  def forAll[K, V](m: Map[K, V], f: ((K, V)) => Boolean): Boolean = {
  // Check if  predicate for known items
  val knownPredicateHolds =  m.filter((a, MapValue(value, present)) => present).forall((k, MapValue(value, present)) => f(k,v))

  // Check if the predicate holds for the unknown item
  val (unknownkey , MapValue(unknownValue, unknownPresence)) = unknownItem
  val unknownPredicateHolds = if unknownPresence then f((unknownKey, unknownValue)) else true 

  // Update the map's invariant
  val newInvariant: ((K, V, Boolean)) => Boolean = (item: (K, MapVAlue(V, Boolean))) => f(item) && unknownPredicateHolds

  //defining layers 
  // should the sudocode look something like this ?
  // we keep in memory all the previous layers
  def checkLayers(layers: List[Map[K, (V, Boolean)]], prevInvariant: ((K, V, Boolean)) => Boolean): Boolean = {
    layers match {
      case Nil => true
      case layer :: rest => {
        val layerItems = layer.map { case (k, (v, p)) => (k, v, p) }
        val layerPredicateHolds = layerItems.forall(prevInvariant)
        layerPredicateHolds && checkLayers(rest, newInvariant)
      }
    }
  }

  // Check the invariant for all layers efterwards then adding the new map as a layer

}
  





}





    

