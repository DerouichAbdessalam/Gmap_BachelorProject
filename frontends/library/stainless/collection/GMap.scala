
package stainless.collection

import stainless.annotation._
/**
  * representation of a ghost map
  * WEEK 1 : get the invariants right
  * meeting : get hints about the invariants  
  * WEEK 2 : implement the methods 
  * in the following we will work as if f has an attribute space 
  * 
  * 
  * the function f will keep track of the known items 
  * examples of implementations of f will be given in examples 
  * f = realm of known 
  */
final case class GMap[A, B](length : Int , f: A => (B, Boolean), uknownInvariant : (A,B) => Boolean){



    val uknownItem = f.uknownItem
    /**
      * get implem 
      *
      * @param k : key value
      */
    def get(k: A): Option[(B, Boolean)] = {
        ???
    }ensuring{ res =>
        f.space.size <= length && {
            res match   
                case Option((a,b)) => (a,b) == f(k)
                case None => 
                    uknownInvariant(f.uknown)
        }

    }

    /**
      * set implem 
      *
      * @param k : key 
      * @param v : value associated
      */
    def set(k : A, v : B): GMap[A, B] = {
        val getRes = get(k)
        val incr = ite(getRes == None || !getRes.get()._2 , 1, 0)
        GMap(length + incr, f.orElse( k => (v, true)),uknownInvariant)
    }ensuring{
        res =>
            val getRes = get(k)
            val incr = ite(getRes == None || !getRes.get()._2, 1, 0)
            ite(incr, res.length == length + 1, res.length == length) && {
                val knownSpaceDiff =  res.f.space.diff(f.space)
                val oldSpaceInclusion =res.f.space.contains(f.space) 

                //check equivalence with the condition in paper
                //we suppose here that space is a list 
                oldSpaceInclusion && (knownSpaceDiff == Nil || knownSpaceDiff == List(k))
            }

    }
    

        
    def remove(k : A): GMap[A, B] =  {
        val getRes = get(k)
        val decr = ite(getRes != None && getRes.get()._2 , -1, 0)
        //change arbitrary v from null to something safer maybe f.uknown.
        GMap(length + decr, f.orElse(k => (null, false)), uknownInvariant)
    }ensuring{
        res =>
            val getRes = get(k)
            val incr = ite(getRes != None || getRes.get()._2, -1, 0)
            ite(incr, res.length == length + 1, res.length == length) && {
                val knownSpaceDiff =  f.space.diff(res.f.space)
                val newSpaceInclusion = f.space.contains(res.f.space) 

                //TODO : check equivalence with the condition on paper
                newSpaceInclusion && (knownSpaceDiff == Nil || knownSpaceDiff == List(k))
            }

    }
    


    def forAll(p : (A, B) => Boolean ): Boolean = 
        val knownCheck = k => {
            val getRes = f.get(k)
            val v = getRes.get()._1
            p(k, v)

        }
        f.space.foreach(knownCheck) && p(uknownItem._1, uknownItem._2)
    //should i add an ensuring here ??

}


/**
  * if then else abreviation
  *
  * @param c condition
  * @param t case true 
  * @param f case false 
  */
def ite[A](c : Boolean , t : A, f: A) = if c then t else a
