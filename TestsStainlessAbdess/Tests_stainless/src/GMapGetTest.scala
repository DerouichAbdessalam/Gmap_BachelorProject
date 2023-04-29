import stainless.collection._
import stainless.lang._
import stainless.annotation._

object GMapSpec {

    def dummyInv[A,B]( ukey : A, uvalue: MapValue[B]) = true
    def defaultUnknownItem[A, B](ukey : A, value : B) = ukey -> MapValue(value ,true)      
    
    def testGetEmptyMap(): Unit = {
        val map = GMap[Int,String]( defaultUnknownItem(0, "hey"), dummyInv)
        assert(map.size == 0)
    }
    
}