
package stainless.collection
import org.scalacheck.Arbitrary
import scala.math.BigInt
import stainless.lang._
import stainless.annotation._

  /**
    * defining a map item 
    *
    * @param value
    * @param present
    */
  case class MapValue[B](value : B ,present : Boolean)


  case class MapState[A, B](knownItems: Map[A,MapValue[B]], unknownItemInvariant : MapValue[B] => Boolean, length : BigInt)



class GMap[A, B](uknownItem : (Key, MapValue[B]), unknownItemInvariantInit : (Key, MapValue[B]) => Boolean ){
  
  type Key = A
  type Value = B
  
  //the map state 
  var MapState = MapState[B](Map.Empty[Key, MapValue[B]], unknownItemInvariantInit, 0)
  

  def get(key: Key): (Value, Boolean) = {
    MapState.knownItems().get(key) match {
      case None => {
        // we generate the two fresh values 
        // such that the unkown item invariant applies on the 2 values 
        // MapState.unknownItemInvariant(key, freshV, freshP)
        choose[(Value, Boolean)](t => MapState.unknownItemInvariant(key, MapValue(t._1, t._2)))
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
      MapState.knownItems.filter{case MapItem(_,_,p) => p}.size <= MapState.length

  }

  /**
    * set method for GMAP class 
    */
  def set(key: Key, value: Value): GMap[A, B] = {
    val (oldValue, present) = get(key)
    val newLength = if (present) MapState.length else MapState.length + 1
    
    //no need to create a new state : 
    val newKnownItems = MapState.knownItems.map {
      //case the value was already in the map
      case (k, MapValue(v, p)) if k == key =>
        k -> MapValue(value, true)
      case (k, MapValue(v, p)) =>
        k -> MapValue(v, p)
    } + (key -> MapValue(value, true))
    // this last addition is made to add the eleent to the list of known elems if 
    //it wasn't previously there    

    GMap(uknownItem, unknownItemInvariantInit).ensuring { map =>
      map.MapState.knownItems.keySet == newKnownItems.keySet &&
      newKnownItems.forall {
        case (k, v) => 
          MapState.knownItems.get(k) match {
            case Some(MapValue(value, present)) =>
              (value == v.value && present == v.present) || k == key
            case None =>
              MapState.unknownItemInvariant(v)
          }
      } &&
      map.MapState.length == newLength &&
      map.getItemInvariants == MapState.unknownItemInvariant
    }
  }

  /**
    * remove method implementation 
    */
  def remove(key: Key): GMap[A, B] = {
    val (value, present) = get(key)
    val newLength = MapState.length + (if (present) -1 else 0)
    val newKnownItems = MapState.knownItems.map{ case (k, MapValue(v, p)) =>
      if (k == key) (k, MapValue(value, false))
      else (k, MapValue(v, p && k != key))
    } + (key -> MapValue(value, false))
    GMap[A, B](MapValue[B](value, false), MapState.unknownItemInvariant) ensuring { gmap =>
      gmap.MapState.length == newLength &&
      gmap.MapState.knownItems.keySet == newKnownItems.keySet &&
      gmap.MapState.knownItems.forall{ case (k, MapValue(v, p)) =>
        newKnownItems.get(k).contains(MapValue(v, p))
      } &&
      getItemInvariants(gmap.MapState.unknownItemInvariant)
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



}

    

