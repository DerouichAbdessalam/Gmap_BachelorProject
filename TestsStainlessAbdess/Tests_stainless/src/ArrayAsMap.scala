import stainless.collection._
import stainless.lang._
import stainless.annotation._
import MapValue

//we define an array only containing positive elements only 
class ArrayAsMap {

    def arrayInvariant(key : Int, mapValue: MapValue[Int]): Boolean = value match 
        case MapValue(value, presence) =>  value >= 0 
    
    //what value to choose for the unnown item (does any value verifying the invarisnt work ?)
    def unknownItem : (Int, MapValue[Int]) = (-1 , MapValue(1, 0))
    
    var arrayMap : Gmap(Int, Int) = Gmap(unknownItem, arrayInvariant) 
    
    def mapSetOp() : Unit = 
        arrayMap.set(0, 1)
        

}
