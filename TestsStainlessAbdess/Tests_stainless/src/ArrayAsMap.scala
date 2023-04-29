import stainless.collection._
import stainless.lang._
import stainless.annotation._

//we define an array only containing positive elements only 
object ArrayAsMap {

    def arrayInvariant(key : Int, mapValue: MapValue[Int]): Boolean = mapValue match 
        case MapValue(value, presence) =>  value >= 0 
    
    //what value to choose for the unnown item (does any value verifying the invarisnt work ?)
    //the unknown item is only supposed to contain values that i'll add to it, no reason to think about absent elements
    def unknownItem : (Int, MapValue[Int]) = (-1 , MapValue(1, false))
    
    
    def mapSetOp() : Unit = 
        val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant) 
        val nA = arrayMap.set(0, 1)
        arrayMap.setPost(0,1)

    def mapGetOp() : Unit = 
        val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant) 
        val nA = arrayMap.set(0, 1)
        nA.get(0)
        nA.getPost(0)        
              
}
