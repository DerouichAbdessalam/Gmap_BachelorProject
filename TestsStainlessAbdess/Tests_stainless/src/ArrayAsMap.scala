import stainless.collection._
import stainless.lang._
import stainless.annotation._

//we define an array implemented as a map
object ArrayAsMap {
    // representation of the elements not yet in the map
    def arrayInvariant(key : Int, mapValue: MapValue[Int]): Boolean = mapValue.value >= 0

    //what value to choose for the unnown item (does any value verifying the invarisnt work ?)
    //the unknown item is only supposed to contain values that i'll add to it, no reason to think about absent elements
    def unknownItem : (Int, MapValue[Int]) = (-1 , MapValue(1, false))
    

    def mapGetOpPresentElem() : Unit = 
        val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant) 
        val nA = arrayMap.set(0, 1)
        //a get on a present key item should return the associated value 
        assert(nA.get(0)._1 == 1)
        nA.getPost(0)   

    def mapGetOpAbsentElem() : Unit = 
        val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant) 
        val nA = arrayMap.set(0, 1)
        //a get on an absent key should return a value that verifies the unknown item invariant
        val (res, pres) = nA.get(2)
        assert(arrayInvariant(2, MapValue(res, pres)))
        nA.getPost(0)   

    def mapSetOp() : Unit = 
        val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant) 
        val nA = arrayMap.set(0, 1)
        arrayMap.setPost(0,1)

    def mapRemoveOp() : Unit = 
        val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant) 
        val nA = arrayMap.set(0, 1)
        val n = arrayMap.remove(0)
        nA.removePost(0)

    
     
    def positiveMapForAllOp() : Unit = {
        val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant) 
        val nA = arrayMap.set(0, 1)
        val res = nA.forAll((x: Int, mapV : MapValue[Int]) => mapV.value >= 0)
        assert(res)
    }


    def negativeMapForAllOp() : Unit = {
        val arrayMap : GMap[Int, Int] = GMap(unknownItem, arrayInvariant) 
        val nA = arrayMap.set(0, -1)
        val res = nA.forAll((x: Int, mapV : MapValue[Int]) => mapV.value >= 0)
        assert(!res)
    }
}


