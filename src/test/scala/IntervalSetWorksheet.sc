package com.youdevise.albatross

import Intervals._
import Bounds._

object IntervalSetWorksheet {

  open(0) to open(1)                              //> res0: com.youdevise.albatross.ContinuousInterval[Int] = (0...1)
  Interval(open(0) to open(1))                    //> res1: com.youdevise.albatross.Interval[Int] = (0...1)
  
	val is = (open(0) to open(10)) union (open(20) to open(30)) union (open(25) to open(34))
                                                  //> is  : com.youdevise.albatross.Interval[Int] = (0...10) ∪ (20...34)

  // Intersection
	is intersect (open(5) to open(24))        //> res2: com.youdevise.albatross.Interval[Int] = (5...10) ∪ (20...24)
                                                  
  Interval(open(0) to open(1), open(4) to open(8), open(10) to open(11), open(14) to open(16)) intersect Interval(open(2) to open(3), open(3) to open(5), open(6) to open(7), open(9) to open(15))
                                                  //> res3: com.youdevise.albatross.Interval[Int] = (4...5) ∪ (6...7) ∪ (10...
                                                  //| 11) ∪ (14...15)
                                                  
  // Unions
  is union (open(3) to open(21))                  //> res4: com.youdevise.albatross.Interval[Int] = (0...34)
  
  // Intersection and union
  (open(0) to open(10)) intersect (open(2) to open(3)) union (open(7) to open(8))
                                                  //> res5: com.youdevise.albatross.Interval[Int] = (2...3) ∪ (7...8)
  ((open(0) to open(10)) intersect (open(2) to open(3))) union (open(7) to open(8))
                                                  //> res6: com.youdevise.albatross.Interval[Int] = (2...3) ∪ (7...8)
  (open(0) to open(10)) intersect ((open(2) to open(3)) union (open(7) to open(8)))
                                                  //> res7: com.youdevise.albatross.Interval[Int] = (2...3) ∪ (7...8)
                                                  
  // Enclosure
  is enclosesInterval (open(2) to open(3))        //> res8: Boolean = true
  is enclosesInterval Interval(open(2) to open(3), open(21) to open(25))
                                                  //> res9: Boolean = true
}