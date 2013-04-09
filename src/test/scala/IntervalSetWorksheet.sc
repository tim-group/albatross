package com.youdevise.example

import scala.util.control.Exception.allCatch
import com.youdevise.albatross.IntervalSet
import com.youdevise.albatross.Bounds._
import com.youdevise.albatross.Discrete.{IntDomain, CharDomain, intervalSet2DiscreteInterval}

object IntervalSetWorksheet {
  /*
  * An interval set is a set defined as all of the values lying between two bounds - for example,
  * all the integers from 1 to 10 inclusive.
  */
  closed(1) to closed(10)                         //> res0: com.youdevise.albatross.Continuous[Int] = [1...10]
  
  /*
  * The bounds of an interval set may be closed, in which case the set includes the bounding value,
  * or open, in which case it does not.
  *
  * If the domain of the interval set is discrete, we can list out the items contained in the set.
  */
  (closed(1) to open(10)).toList                  //> res1: <error> = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  /*
  * An interval set may be unbounded at either its lower or its upper bound.
  * The following contains all the negative integers:
  */
  unbounded[Int] to open(0)                       //> res2: com.youdevise.albatross.Continuous[Int] = ∞...0)
  
  allCatch either { (unbounded[Int] to open(0)).toList }
                                                  //> res3: <error> = Left(java.lang.UnsupportedOperationException: Cannot convert
                                                  //|  unbounded interval to list)
  
  /*
  * Unbounded intervals in discrete domains can be streamed.
  */
  val s = (unbounded[Int] to open(0)).toReverseStream
                                                  //> s  : Stream[Int] = Stream(-1, ?)
  s.take(5).toList                                //> res4: List[Int] = List(-1, -2, -3, -4, -5)
  
  /*
  * An interval set can be used to test whether or not a value is in the set.
  */
  (closed('a') to closed('z')).encloses('q')      //> res5: Boolean = true
  (closed('a') to closed('m')).encloses('q')      //> res6: Boolean = false
  
  (closed("apple") to closed("metallurgy")).encloses("metallic")
                                                  //> res7: Boolean = true
  (closed("apple") to closed("metallurgy")).encloses("metaphorical")
                                                  //> res8: Boolean = false
  
  /*
  * An interval set may not be continuous; parts of the range between the lower and upper bounds
  * may be missing.
  */
  ((closed(1) to closed(3)) union (closed(7) to closed(9))).toList
                                                  //> res9: <error> = List(1, 2, 3, 7, 8, 9)
  ((closed(1) to closed(10)) complement (closed(3) to closed(7))).toList
                                                  //> res10: <error> = List(1, 2, 8, 9, 10)
                                                  
  /*
  * An interval set may even be empty, if it encloses no values.
  */
  (open(0) to open(0)).isEmpty                    //> res11: Boolean = true
  ((closed(1) to closed(10)) intersect (closed(-10) to closed(-1))).isEmpty
                                                  //> res12: Boolean = true
                                                  
  /*
  * We can test to see whether one interval set encloses another.
  */
  val a = IntervalSet(open(0) to open(10), open(20) to open(30))
                                                  //> a  : com.youdevise.albatross.IntervalSet[Int] = (0...10) √ (20...30)
  a enclosesInterval (open(5) to open(9))         //> res13: Boolean = true
  a enclosesInterval (open(5) to open(15))        //> res14: Boolean = false
  a enclosesInterval IntervalSet(open(5) to open(9), open(23) to open(29))
                                                  //> res15: Boolean = true
                                                  
  /*
  * An interval can be used as a predicate for a filter
  */
  Seq(1.0, 2.3, 3.5, 4.2, 5.7).filter(open(2.2) to open(4.2))
                                                  //> res16: Seq[Double] = List(2.3, 3.5)
}